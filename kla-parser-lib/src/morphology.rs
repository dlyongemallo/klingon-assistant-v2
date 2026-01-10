// Multi-hypothesis Klingon morphological analyser.
//
// For each word we generate three families of hypotheses:
// Infrastructure (constants, suffix enumeration) is defined here;
// verb/noun/pronoun hypothesis generators follow in subsequent commits.
#![allow(dead_code)]
//
//   A. Whole-word (dictionary lookup)
//   B. Verb (prefix + suffix chain)
//   C. Noun (suffix chain only)
//
// Suffix enumeration uses an iterative work-queue that branches at every
// suffix-type position, producing ALL valid decompositions rather than a
// single greedy "best" parse.

use crate::confidence;
use crate::dictionary::Dictionary;
use crate::types::*;

// ---------------------------------------------------------------------------
// Suffix / prefix constants
// ---------------------------------------------------------------------------

// Verb prefixes (29 total).
const VERB_PREFIXES: &[&str] = &[
    "", "bI", "bo", "che", "cho", "Da", "DI", "Du", "gho", "HI", "jI", "ju", "lI", "lu", "ma",
    "mu", "nI", "nu", "pe", "pI", "qa", "re", "Sa", "Su", "tI", "tu", "vI", "wI", "yI",
];

// Noun suffix types (innermost → outermost order in the arrays, but we strip
// outermost first during parsing).
const NOUN_TYPE1: &[&str] = &["'a'", "Hom", "oy"];
const NOUN_TYPE2: &[&str] = &["pu'", "Du'", "mey"];
const NOUN_TYPE3: &[&str] = &["qoq", "Hey", "na'"];
const NOUN_TYPE4: &[&str] = &[
    "wIj", "wI'", "maj", "ma'", "lIj", "lI'", "raj", "ra'", "Daj", "chaj", "vam", "vetlh",
];
const NOUN_TYPE5: &[&str] = &["Daq", "vo'", "mo'", "vaD", "'e'"];

// Verb suffix types.
const VERB_TYPE_R_UNDO: &[&str] = &["Ha'"];
const VERB_TYPE1: &[&str] = &["'egh", "chuq"];
const VERB_TYPE2: &[&str] = &["nIS", "qang", "rup", "beH", "vIp"];
const VERB_TYPE3: &[&str] = &["choH", "qa'"];
const VERB_TYPE4: &[&str] = &["moH"];
const VERB_TYPE5: &[&str] = &["lu'", "laH", "luH", "la'"];
const VERB_TYPE6: &[&str] = &["chu'", "bej", "ba'", "law'"];
const VERB_TYPE7: &[&str] = &["pu'", "ta'", "taH", "lI'"];
const VERB_TYPE8: &[&str] = &["neS"];
const VERB_TYPE_R_REFUSAL: &[&str] = &["Qo'"];
const VERB_TYPE9: &[&str] = &[
    "DI'", "chugh", "pa'", "vIS", "mo'", "bogh", "meH", "'a'", "jaj", "wI'", "ghach",
];

// Rovers (verb only, can appear after any suffix type position).
const ROVER_NEGATION: &str = "be'";
const ROVER_EMPHATIC: &str = "qu'";

// Noun type 4 possessive suffix subsets (being vs. non-being).
const POSSESSIVE_BEING: &[&str] = &["wI'", "lI'", "ma'", "ra'"];
const POSSESSIVE_NON_BEING: &[&str] = &["wIj", "lIj", "maj", "raj"];

const KLINGON_VOWELS: &[char] = &['a', 'e', 'I', 'o', 'u'];

// The 8 Klingon pronouns that can act as copular verbs (TKD 6.3).
const PRONOUNS: &[&str] = &[
    "jIH", "SoH", "ghaH", "'oH", "maH", "tlhIH", "chaH", "bIH",
];

// Only {-laH} from verb type 5 is permitted on copular pronouns.
const COPULA_TYPE5: &[&str] = &["laH"];

// Number-forming elements: digit + multiplier = compound number.
// e.g., {loS} + {maH} = {loSmaH} (40).
const NUMBER_DIGITS: &[&str] = &[
    "pagh", "wa'", "cha'", "wej", "loS", "vagh", "jav", "Soch", "chorgh", "Hut",
];
const NUMBER_MULTIPLIERS: &[&str] = &[
    "maH", "vatlh", "SaD", "SanID", "netlh", "bIp", "'uy'",
];

// ---------------------------------------------------------------------------
// Sub-component parsing for whole-word entries
// ---------------------------------------------------------------------------

/// Parse a dictionary components string (e.g., "{nuH:n:1}, {-mey:n}") into a
/// Vec<Component>. Skips entries containing " or " (ambiguous alternative
/// analyses) and entries with non-standard notation.
fn parse_components_str(components: &str) -> Vec<Component> {
    // Skip ambiguous entries with alternative analyses.
    if components.contains(" or ") {
        return vec![];
    }

    let mut result = Vec::new();
    let mut rest = components;

    while let Some(open) = rest.find('{') {
        rest = &rest[open + 1..];
        let close = match rest.find('}') {
            Some(c) => c,
            None => break,
        };
        let inner = &rest[..close];
        rest = &rest[close + 1..];

        // Split on first ':' to get entry_name and pos_part.
        let colon = match inner.find(':') {
            Some(c) => c,
            None => continue,
        };
        let entry_name = &inner[..colon];
        let pos_part = &inner[colon + 1..];

        // Skip sentence references and comparative patterns.
        if pos_part.starts_with("sen") || entry_name.starts_with("...") {
            return vec![];
        }

        // Skip entries with internal tags like "hyp,nolink" in the pos.
        if pos_part.contains("hyp") {
            return vec![];
        }

        // Determine role from entry_name.
        let (role, text) = if entry_name.starts_with('-') {
            (MorphemeRole::Suffix, entry_name[1..].to_string())
        } else if entry_name.ends_with('-') {
            (MorphemeRole::Prefix, entry_name[..entry_name.len() - 1].to_string())
        } else {
            (MorphemeRole::Stem, entry_name.to_string())
        };

        // Parse canonical_pos and homophone from pos_part.
        // Format: "n", "n:1", "v:1", etc. Filter out internal tags.
        let colon_parts: Vec<&str> = pos_part.split(':').collect();
        let mut homophone: Option<u8> = None;
        let mut canonical_parts = Vec::new();

        for (i, part) in colon_parts.iter().enumerate() {
            let comma_tokens: Vec<&str> = part.split(',').collect();
            let mut kept = Vec::new();
            for token in &comma_tokens {
                let t = token.trim();
                if t.is_empty() {
                    continue;
                }
                if t.chars().next().map_or(false, |c| c.is_ascii_digit()) {
                    if let Ok(n) = t.trim_end_matches(|c: char| !c.is_ascii_digit()).parse::<u8>() {
                        homophone = Some(n);
                    }
                    kept.push(t);
                } else if i == 0 && kept.is_empty() {
                    // Always keep the base POS.
                    kept.push(t);
                } else {
                    // Keep non-internal tags.
                    const INTERNAL: &[&str] = &[
                        "alt", "klcp1", "t_c", "i_c", "is", "t", "i", "ambi", "anim",
                        "being", "body", "deiv", "deri", "deriv", "epithet", "extcan",
                        "fic", "food", "idiom", "nodict", "noanki", "person", "place",
                        "plural", "reg", "terran", "weap", "inhpl", "inhps", "slang",
                        "archaic", "hyp", "nolink", "suff", "pref",
                    ];
                    if !INTERNAL.contains(&t) {
                        kept.push(t);
                    }
                }
            }
            if !kept.is_empty() {
                canonical_parts.push(kept.join(","));
            }
        }

        // Drop "pro"/"num"/"name" when homophone is present.
        if homophone.is_some() {
            for part in &mut canonical_parts {
                let filtered: Vec<&str> = part
                    .split(',')
                    .filter(|s| *s != "pro" && *s != "num" && *s != "name")
                    .collect();
                *part = filtered.join(",");
            }
            canonical_parts.retain(|p| !p.is_empty());
        }

        let pos = canonical_parts.join(":");

        result.push(Component {
            text,
            entry_name: entry_name.to_string(),
            pos,
            homophone,
            role,
            sub_components: vec![],
        });
    }

    result
}

// ---------------------------------------------------------------------------
// Partial parse state used during suffix enumeration
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct Partial {
    /// Remaining text to analyse (potential stem).
    remainder: String,
    /// Suffixes stripped so far (outermost first, reversed later).
    suffixes: Vec<(String, SuffixMeta)>,
}

#[derive(Debug, Clone)]
struct SuffixMeta {
    /// "n" or "v".
    pos: &'static str,
    /// True if this is a rover.
    is_rover: bool,
    /// True if this is {-ghach}.
    is_ghach: bool,
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a single word, returning all hypotheses sorted by confidence.
pub fn parse_word(word: &str, dict: &Dictionary) -> WordParse {
    let mut hypotheses = Vec::new();

    // A. Whole-word hypotheses.
    if let Some(entries) = dict.lookup(word) {
        for entry in entries {
            let sub_components = if !entry.components.is_empty() {
                parse_components_str(&entry.components)
            } else {
                vec![]
            };
            let comp = Component {
                text: word.to_string(),
                entry_name: word.to_string(),
                pos: entry.canonical_pos.clone(),
                homophone: if entry.homophone > 0 {
                    Some(entry.homophone)
                } else {
                    None
                },
                role: MorphemeRole::Stem,
                sub_components,
            };
            hypotheses.push(Hypothesis {
                components: vec![comp],
                confidence: 0.0, // Scored later.
                parse_type: ParseType::WholeWord,
                warnings: vec![],
            });
        }
    }

    // B. Verb hypotheses (added in next commit).

    // C. Noun hypotheses (added in next commit).

    // F. Unknown fallback (always present).
    if hypotheses.is_empty() || !hypotheses.iter().any(|h| h.parse_type == ParseType::WholeWord) {
        hypotheses.push(Hypothesis {
            components: vec![Component {
                text: word.to_string(),
                entry_name: word.to_string(),
                pos: String::new(),
                homophone: None,
                role: MorphemeRole::Stem,
                sub_components: vec![],
            }],
            confidence: 0.0,
            parse_type: ParseType::Unknown,
            warnings: vec![],
        });
    }

    // Score and sort.
    for h in &mut hypotheses {
        h.confidence = confidence::score(h, dict);
    }
    hypotheses.sort_by(confidence::compare);

    // Deduplicate identical component sequences.
    dedup_hypotheses(&mut hypotheses);

    WordParse {
        word: word.to_string(),
        hypotheses,
    }
}

// ---------------------------------------------------------------------------
// Stem resolution helper
// ---------------------------------------------------------------------------

/// Resolve all stem POS and homophone variants from dictionary entries.
/// Returns one (canonical_pos, homophone) pair per matching entry. If no entry
/// matches `preferred_base_pos`, falls back to the first entry.
fn resolve_stem_all(dict: &Dictionary, stem: &str, preferred_base_pos: &str) -> Vec<(String, Option<u8>)> {
    let entries = match dict.lookup(stem) {
        Some(es) => es,
        None => return vec![(String::new(), None)],
    };

    // Collect all entries matching the preferred POS.
    let matching: Vec<_> = entries
        .iter()
        .filter(|e| e.pos == preferred_base_pos)
        .collect();

    let chosen = if matching.is_empty() {
        // Fallback: just use the first entry.
        vec![entries.first().unwrap()]
    } else {
        matching
    };

    chosen
        .into_iter()
        .map(|e| {
            (
                e.canonical_pos.clone(),
                if e.homophone > 0 {
                    Some(e.homophone)
                } else {
                    None
                },
            )
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Generic suffix enumeration (work-queue approach)
// ---------------------------------------------------------------------------

/// Enumerate all valid suffix decompositions for `word` given ordered suffix
/// types (outermost first). Returns a list of (stem, suffixes) pairs where
/// suffixes are in outermost-first order.
fn enumerate_suffixes(
    word: &str,
    suffix_types: &[&[&str]],
    pos: &'static str,
    allow_rovers: bool,
) -> Vec<(String, Vec<(String, SuffixMeta)>)> {
    // Start with just the full word, no suffixes stripped.
    let mut queue: Vec<Partial> = vec![Partial {
        remainder: word.to_string(),
        suffixes: vec![],
    }];

    for suffix_type in suffix_types {
        let mut next_queue = Vec::new();

        for partial in &queue {
            // Branch 1: no suffix of this type (pass through).
            next_queue.push(partial.clone());

            // Branch 2: try each suffix option.
            for &suf in *suffix_type {
                if suf.is_empty() {
                    continue;
                }
                if partial.remainder.ends_with(suf) && partial.remainder.len() > suf.len() {
                    let new_remainder =
                        partial.remainder[..partial.remainder.len() - suf.len()].to_string();
                    let mut new_suffixes = partial.suffixes.clone();
                    let is_ghach = suf == "ghach";
                    new_suffixes.push((
                        suf.to_string(),
                        SuffixMeta {
                            pos,
                            is_rover: false,
                            is_ghach,
                        },
                    ));

                    // Optionally strip rovers at this position.
                    if allow_rovers {
                        let rover_variants =
                            strip_rovers_all_combos(&new_remainder, pos);
                        for (rover_rem, rover_suffixes) in rover_variants {
                            let mut combined = new_suffixes.clone();
                            combined.extend(rover_suffixes);
                            next_queue.push(Partial {
                                remainder: rover_rem,
                                suffixes: combined,
                            });
                        }
                    } else {
                        next_queue.push(Partial {
                            remainder: new_remainder,
                            suffixes: new_suffixes,
                        });
                    }
                }
            }

            // Also try stripping rovers without a suffix at this position.
            if allow_rovers {
                let rover_variants =
                    strip_rovers_all_combos(&partial.remainder, pos);
                for (rover_rem, rover_suffixes) in &rover_variants {
                    if !rover_suffixes.is_empty() {
                        let mut new_suffixes = partial.suffixes.clone();
                        new_suffixes.extend(rover_suffixes.clone());
                        next_queue.push(Partial {
                            remainder: rover_rem.clone(),
                            suffixes: new_suffixes,
                        });
                    }
                }
            }
        }

        // Prune empty remainders and deduplicate.
        next_queue.retain(|p| !p.remainder.is_empty());
        dedup_partials(&mut next_queue);

        queue = next_queue;
    }

    // Convert to output format.
    queue
        .into_iter()
        .map(|p| (p.remainder, p.suffixes))
        .collect()
}

/// Generate all rover-stripping combinations at a single position.
/// Returns variants: (no rovers), ({-be'} only), ({-qu'} only), ({-be'} then {-qu'}).
fn strip_rovers_all_combos(
    remainder: &str,
    pos: &'static str,
) -> Vec<(String, Vec<(String, SuffixMeta)>)> {
    let mut results = Vec::new();

    // No rovers.
    results.push((remainder.to_string(), vec![]));

    // {-qu'} only.
    if remainder.ends_with(ROVER_EMPHATIC) && remainder.len() > ROVER_EMPHATIC.len() {
        let r1 = remainder[..remainder.len() - ROVER_EMPHATIC.len()].to_string();
        results.push((
            r1.clone(),
            vec![(
                ROVER_EMPHATIC.to_string(),
                SuffixMeta {
                    pos,
                    is_rover: true,
                    is_ghach: false,
                },
            )],
        ));

        // {-be'} then {-qu'}.
        if r1.ends_with(ROVER_NEGATION) && r1.len() > ROVER_NEGATION.len() {
            let r2 = r1[..r1.len() - ROVER_NEGATION.len()].to_string();
            results.push((
                r2,
                vec![
                    (
                        ROVER_EMPHATIC.to_string(),
                        SuffixMeta {
                            pos,
                            is_rover: true,
                            is_ghach: false,
                        },
                    ),
                    (
                        ROVER_NEGATION.to_string(),
                        SuffixMeta {
                            pos,
                            is_rover: true,
                            is_ghach: false,
                        },
                    ),
                ],
            ));
        }
    }

    // {-be'} only.
    if remainder.ends_with(ROVER_NEGATION) && remainder.len() > ROVER_NEGATION.len() {
        let r1 = remainder[..remainder.len() - ROVER_NEGATION.len()].to_string();
        results.push((
            r1,
            vec![(
                ROVER_NEGATION.to_string(),
                SuffixMeta {
                    pos,
                    is_rover: true,
                    is_ghach: false,
                },
            )],
        ));
    }

    results
}

/// Remove duplicate partials (same remainder + same suffix texts).
fn dedup_partials(partials: &mut Vec<Partial>) {
    let mut seen = std::collections::HashSet::new();
    partials.retain(|p| {
        let key = format!(
            "{}|{}",
            p.remainder,
            p.suffixes
                .iter()
                .map(|(s, _)| s.as_str())
                .collect::<Vec<_>>()
                .join(",")
        );
        seen.insert(key)
    });
}

/// Remove hypotheses with identical component sequences.
fn dedup_hypotheses(hypotheses: &mut Vec<Hypothesis>) {
    let mut seen = std::collections::HashSet::new();
    hypotheses.retain(|h| {
        let key = format!(
            "{}|{}",
            h.parse_type as u8,
            h.components
                .iter()
                .map(|c| format!(
                    "{}:{}:{}",
                    c.entry_name,
                    c.pos,
                    c.homophone.map_or(0, |h| h)
                ))
                .collect::<Vec<_>>()
                .join(",")
        );
        seen.insert(key)
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dictionary::Dictionary;

    fn dict() -> Dictionary {
        Dictionary::new()
    }

    #[test]
    fn test_qapla_whole_word() {
        let d = dict();
        let result = parse_word("Qapla'", &d);
        assert!(
            !result.hypotheses.is_empty(),
            "should have at least one hypothesis"
        );
        let top = &result.hypotheses[0];
        assert_eq!(top.parse_type, ParseType::WholeWord);
        assert_eq!(top.components[0].entry_name, "Qapla'");
    }
}
