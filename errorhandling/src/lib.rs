pub enum ACompilationError {
    SyntaxError(ASyntaxError),
    ResolutionError(AResolutionError),
    VerifyError(AResolutionError),
}

pub enum ASyntaxError {}
pub enum AResolutionError {}
pub enum AVerifyError {}
