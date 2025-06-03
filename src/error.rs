use crate::lexer::{Loc, SourceId, Token};
use ariadne::{ColorGenerator, Config, Label, Report, ReportKind, Source};
use std::collections::HashMap;
use std::fmt;

// --- Ariadne Cache ---

pub struct AriadneCache {
    sources: HashMap<SourceId, Source>,
    filenames: HashMap<SourceId, String>,
}

impl AriadneCache {
    pub fn new() -> Self {
        AriadneCache {
            sources: HashMap::new(),
            filenames: HashMap::new(),
        }
    }

    pub fn add_source(&mut self, id: SourceId, filename: String, content: String) {
        self.sources.insert(id, Source::from(content));
        self.filenames.insert(id, filename);
    }

    pub fn get_filename(&self, id: &SourceId) -> Option<&String> {
        self.filenames.get(id)
    }
}

#[allow(refining_impl_trait)]
impl ariadne::Cache<SourceId> for &AriadneCache {
    type Storage = String;

    fn fetch(&mut self, id: &SourceId) -> Result<&Source, Box<dyn fmt::Debug + '_>> {
        self.sources
            .get(id)
            .ok_or_else(|| Box::new(format!("Source not found: {:?}", id)) as Box<dyn fmt::Debug>)
    }

    fn display<'a>(&self, id: &'a SourceId) -> Option<Box<dyn fmt::Display + 'a>> {
        self.filenames
            .get(id)
            .map(|f| Box::new(f.clone()) as Box<dyn fmt::Display + 'a>)
    }
}

impl ariadne::Span for Loc {
    type SourceId = SourceId;
    fn source(&self) -> &Self::SourceId {
        &self.source
    }
    fn start(&self) -> usize {
        self.span.start
    }
    fn end(&self) -> usize {
        self.span.end
    }
}

// --- Parser Errors ---

// Using Chumsky's Rich error directly for parsing errors.
// Alias for convenience if needed.
pub type RichParseError<'a> = chumsky::error::Rich<'a, Token, Loc>;

pub fn report_parser_errors(cache: &AriadneCache, errors: Vec<RichParseError<'_>>) {
    let mut colors = ColorGenerator::new();
    for err in errors {
        let report_kind = ReportKind::Error;

        let mut report_builder = Report::build(report_kind, err.span().clone())
            .with_config(
                Config::default()
                    .with_cross_gap(false)
                    .with_char_set(ariadne::CharSet::Ascii),
            ) // Use Ascii for wider compatibility
            .with_message(format!("{}", err.reason())); // Main message from reason

        match err.reason() {
            chumsky::error::RichReason::ExpectedFound { expected, found } => {
                let msg = String::from("Syntax error"); // Default message

                report_builder = report_builder.with_label(
                    Label::new(err.span().clone())
                        .with_message(format!(
                            "Unexpected token: {}{}, expected {}",
                            if found.is_some() { "found " } else { "" },
                            found
                                .clone()
                                .map(|t| format!("`{}`", t.to_string()))
                                .unwrap_or_else(|| "end of input".to_string()),
                            if expected.len() == 0 {
                                "something else".to_string()
                            } else {
                                expected
                                    .iter()
                                    .map(|opt_t| format!("`{}`", opt_t.to_string()))
                                    .collect::<Vec<_>>()
                                    .join(" or ")
                            }
                        ))
                        .with_color(colors.next()),
                );
                report_builder = report_builder.with_message(msg);
            }
            chumsky::error::RichReason::Custom(msg) => {
                report_builder = report_builder.with_label(
                    Label::new(err.span().clone())
                        .with_message(msg)
                        .with_color(colors.next()),
                );
                report_builder = report_builder.with_message("Syntax error");
            }
        }

        report_builder
            .finish()
            .eprint(cache)
            .unwrap_or_else(|e| eprintln!("Error reporting error: {:?}", e));
    }
}

fn label_to_code(label: &str) -> u16 {
    // Simple hash for an error code, replace with actual error codes
    label.chars().map(|c| c as u16).sum::<u16>() % 1000 + 1 // E001-E999
}

// --- Compiler Errors ---

#[derive(Debug, Clone)]
#[allow(dead_code)] // Allow unused for example errors
pub enum CompilerErrorType {
    TypeMismatch {
        expected: String,
        found: String,
    },
    UndefinedVariable {
        name: String,
    },
    UndefinedMember {
        object_type: String,
        member_name: String,
    },
    InvalidOperation {
        op: String,
        type_name: String,
    },
    ArityMismatch {
        expected: usize,
        found: usize,
        callee_name: String,
    },
    ImmutableAssignment {
        var_name: String,
    },
    DuplicateDefinition {
        name: String,
    },
    InvalidAwait {
        context: String,
    },
    YieldOutsideGenerator,
    ReturnOutsideFunction,
    BreakOutsideLoop,
    ContinueOutsideLoop,
    FeatureNotImplemented(String),
    InternalCompilerError(String),
}

#[derive(Debug, Clone)]
pub struct CompilationError {
    pub error_type: CompilerErrorType,
    pub loc: Loc,
    pub notes: Vec<(String, Option<Loc>)>,
}

impl CompilationError {
    pub fn new(error_type: CompilerErrorType, loc: Loc) -> Self {
        Self {
            error_type,
            loc,
            notes: Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub fn with_note(mut self, message: String, loc: Option<Loc>) -> Self {
        self.notes.push((message, loc));
        self
    }

    pub fn report(&self, cache: &AriadneCache, source_id: SourceId) {
        let mut colors = ColorGenerator::new();
        let main_color = colors.next();

        let (code, message) = self.error_details();

        let mut report = Report::build(ReportKind::Error, self.loc.clone())
            .with_code(code)
            .with_message(message.clone())
            .with_label(
                Label::new(self.loc.clone())
                    .with_message(self.primary_message())
                    .with_color(main_color),
            );

        for (note_msg, note_loc) in &self.notes {
            if let Some(loc) = note_loc {
                report = report.with_label(
                    Label::new(loc.clone())
                        .with_message(note_msg)
                        .with_color(colors.next()),
                );
            } else {
                report = report.with_note(note_msg);
            }
        }

        report
            .finish()
            .eprint(cache)
            .unwrap_or_else(|e| eprintln!("Error reporting error: {:?}", e));
    }

    fn error_details(&self) -> (String, String) {
        match &self.error_type {
            CompilerErrorType::TypeMismatch { .. } => {
                ("E001".to_string(), "Type Mismatch".to_string())
            }
            CompilerErrorType::UndefinedVariable { .. } => {
                ("E002".to_string(), "Undefined Variable".to_string())
            }
            CompilerErrorType::InvalidOperation { .. } => {
                ("E003".to_string(), "Invalid Operation".to_string())
            }
            // Add more error codes and general messages
            _ => ("E999".to_string(), "Compilation Error".to_string()),
        }
    }

    fn primary_message(&self) -> String {
        match &self.error_type {
            CompilerErrorType::TypeMismatch { expected, found } => {
                format!("Expected type `{}`, but found type `{}`", expected, found)
            }
            CompilerErrorType::UndefinedVariable { name } => {
                format!("Cannot find variable `{}` in this scope", name)
            }
            CompilerErrorType::UndefinedMember {
                object_type,
                member_name,
            } => {
                format!(
                    "No member `{}` found for type `{}`",
                    member_name, object_type
                )
            }
            CompilerErrorType::InvalidOperation { op, type_name } => {
                format!("Cannot apply operator `{}` to type `{}`", op, type_name)
            }
            CompilerErrorType::ArityMismatch {
                expected,
                found,
                callee_name,
            } => {
                format!(
                    "Function `{}` expected {} arguments, but found {}",
                    callee_name, expected, found
                )
            }
            CompilerErrorType::ImmutableAssignment { var_name } => {
                format!(
                    "Cannot assign to immutable variable `{}`. Try declaring with `let`.",
                    var_name
                )
            }
            CompilerErrorType::DuplicateDefinition { name } => {
                format!("`{}` is already defined in this scope", name)
            }
            CompilerErrorType::InvalidAwait { context } => {
                format!(
                    "`.await` can only be used inside an async function, not in `{}`",
                    context
                )
            }
            CompilerErrorType::YieldOutsideGenerator => {
                "`yield` can only be used inside a generator function (`gen fn`)".to_string()
            }
            CompilerErrorType::ReturnOutsideFunction => {
                "`return` can only be used inside a function".to_string()
            }
            CompilerErrorType::BreakOutsideLoop => {
                "`break` can only be used inside a loop".to_string()
            }
            CompilerErrorType::ContinueOutsideLoop => {
                "`continue` can only be used inside a loop".to_string()
            }
            CompilerErrorType::FeatureNotImplemented(feature) => {
                format!("Feature not implemented: {}", feature)
            }
            CompilerErrorType::InternalCompilerError(msg) => {
                format!("Internal compiler error: {}", msg)
            }
        }
    }
}
