use crate::db::SourceDb;
use crate::files::{SourceFileId, Utf8PathBuf};
use crate::Span;
use codespan_lsp::byte_span_to_range;
pub use codespan_reporting::diagnostic as cs;
use codespan_reporting::files::Error as CsError;
use codespan_reporting::term;
use lsp_types::{Diagnostic as LSPDiagnostic, Range as LSPRange};

pub use cs::Severity;
use std::ops::Range;

use std::sync::Arc;
use term::termcolor::{BufferWriter, ColorChoice};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}
impl Diagnostic {
    pub fn into_cs(self) -> cs::Diagnostic<SourceFileId> {
        cs::Diagnostic {
            severity: self.severity,
            code: None,
            message: self.message,
            labels: self.labels.into_iter().map(Label::into_cs_label).collect(),
            notes: self.notes,
        }
    }
    pub fn error(message: String) -> Self {
        Self {
            severity: Severity::Error,
            message,
            labels: vec![],
            notes: vec![],
        }
    }

    pub fn into_lsps(self, db: &dyn SourceDb) -> Vec<LSPDiagnostic> {
        let lsp_severity = match self.severity {
            Severity::Note => Some(lsp_types::DiagnosticSeverity::INFORMATION),
            Severity::Warning => Some(lsp_types::DiagnosticSeverity::WARNING),
            Severity::Error | Severity::Bug => Some(lsp_types::DiagnosticSeverity::ERROR),
            Severity::Help => Some(lsp_types::DiagnosticSeverity::HINT),
        };

        return self
            .labels
            .into_iter()
            .map(|label| LSPDiagnostic {
                range: label.into_lsp_range(db),
                severity: lsp_severity,
                message: self.message.clone(),
                ..Default::default()
            })
            .collect::<Vec<_>>();
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum LabelStyle {
    Primary,
    Secondary,
}
impl From<LabelStyle> for cs::LabelStyle {
    fn from(other: LabelStyle) -> cs::LabelStyle {
        match other {
            LabelStyle::Primary => cs::LabelStyle::Primary,
            LabelStyle::Secondary => cs::LabelStyle::Secondary,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Label {
    pub style: LabelStyle,
    pub span: Span,
    pub message: String,
}
impl Label {
    /// Create a primary label with the given message. This will underline the
    /// given span with carets (`^^^^`).
    pub fn primary<S: Into<String>>(span: Span, message: S) -> Self {
        Label {
            style: LabelStyle::Primary,
            span,
            message: message.into(),
        }
    }

    /// Create a secondary label with the given message. This will underline the
    /// given span with hyphens (`----`).
    pub fn secondary<S: Into<String>>(span: Span, message: S) -> Self {
        Label {
            style: LabelStyle::Secondary,
            span,
            message: message.into(),
        }
    }

    /// Convert into a [`codespan_reporting::Diagnostic::Label`]
    pub fn into_cs_label(self) -> cs::Label<SourceFileId> {
        cs::Label {
            style: self.style.into(),
            file_id: self.span.file_id,
            range: self.span.into(),
            message: self.message,
        }
    }

    pub fn into_lsp_range(self, db: &dyn SourceDb) -> LSPRange {
        let files = SourceDbWrapper(db);
        return byte_span_to_range(
            &files,
            self.span.file_id,
            Range {
                start: self.span.start,
                end: self.span.end,
            },
        )
        .unwrap();
    }
}

/// Print the given diagnostics to stderr.
pub fn print_diagnostics(db: &dyn SourceDb, diagnostics: &[Diagnostic]) {
    let writer = BufferWriter::stderr(ColorChoice::Auto);
    let mut buffer = writer.buffer();
    let config = term::Config::default();
    let files = SourceDbWrapper(db);

    for diag in diagnostics {
        term::emit(&mut buffer, &config, &files, &diag.clone().into_cs()).unwrap();
    }
    // If we use `writer` here, the output won't be captured by rust's test system.
    eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());
}

/// Format the given diagnostics as a string.
pub fn diagnostics_string(db: &dyn SourceDb, diagnostics: &[Diagnostic]) -> String {
    let writer = BufferWriter::stderr(ColorChoice::Never);
    let mut buffer = writer.buffer();
    let config = term::Config::default();
    let files = SourceDbWrapper(db);

    for diag in diagnostics {
        term::emit(&mut buffer, &config, &files, &diag.clone().into_cs())
            .expect("failed to emit diagnostic");
    }
    std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
}

struct SourceDbWrapper<'a>(pub &'a dyn SourceDb);

impl<'a> codespan_reporting::files::Files<'_> for SourceDbWrapper<'a> {
    type FileId = SourceFileId;
    type Name = Arc<Utf8PathBuf>;
    type Source = Arc<str>;

    fn name(&self, file: SourceFileId) -> Result<Self::Name, CsError> {
        Ok(file.path(self.0))
    }

    fn source(&self, file: SourceFileId) -> Result<Self::Source, CsError> {
        Ok(file.content(self.0))
    }

    fn line_index(&self, file: SourceFileId, byte_index: usize) -> Result<usize, CsError> {
        Ok(file.line_index(self.0, byte_index))
    }

    fn line_range(&self, file: SourceFileId, line_index: usize) -> Result<Range<usize>, CsError> {
        file.line_range(self.0, line_index)
            .ok_or(CsError::LineTooLarge {
                given: line_index,
                max: self.0.file_line_starts(file).len() - 1,
            })
    }
}
