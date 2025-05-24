pub enum ACompilationError {
    SyntaxError(ASyntaxError),
    ResolutionError(AResolutionError),
}

pub enum ASyntaxError {}
pub enum AResolutionError {}
