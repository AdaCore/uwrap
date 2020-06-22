from langkit.lexer import Lexer, LexerToken, Literal, WithText, Pattern, Ignore, WithSymbol, WithTrivia


class Token(LexerToken):
    Module     = WithText()
    Import     = WithText()
    
    Template   = WithText()
    Extends    = WithText()
    Var        = WithText()

    Match      = WithText()
    Else       = WithText()
    Weave      = WithText()
    Wrap       = WithText()    
    With       = WithText()
    
    Visitor    = WithText()
    Lambda     = WithText()
    
    And        = WithText()
    Or         = WithText()
    Not        = WithText()
    
    New        = WithText()
    Fold       = WithText()

    Into       = WithText()
    Over       = WithText()
    All        = WithText()

    LPar       = WithText()
    RPar       = WithText()
    LBrk       = WithText()
    RBrk       = WithText()
    LSBrk      = WithText()
    RSBrk      = WithText()
    Comma      = WithText()
    Colon      = WithText()
    Semicolon  = WithText()
    Arrow      = WithText()
    Dot        = WithText()
    Assign     = WithText()
    Amp        = WithText()
    At         = WithText()

    String     = WithText()
    Comment    = WithTrivia()
    Integer    = WithText()
    Identifier = WithSymbol()

    LitTrue   = WithText()
    LitFalse   = WithText()

template_lexer = Lexer(Token)

template_lexer.add_patterns(
    ("STRING_DBQ", r'\"(\\\"|[^\n\"])*\"'),
    ("STRING_SQ",  r"'(\\'|[^\n'])*'"),
    ("MLSTRING_DBQ", r'\"\"\"([^"]|("[^"])|(""[^"])|\n)*\"\"\"'),
    ("MLSTRING_SQ", r"'''([^']|('[^'])|(''[^'])|\n)*'''"),
)

template_lexer.add_rules(
    (Pattern(r"[ \t\r\n]+"), Ignore()),
    (Pattern(r"#(.?)+"),     Token.Comment),

    (Literal("import"), Token.Import),
    
    (Literal ("template"), Token.Template),
    (Literal ("extends"), Token.Extends),
    (Literal ("var"), Token.Var),

    (Literal("match"), Token.Match),
    (Literal("else"), Token.Else),
    (Literal("wrap"), Token.Wrap),
    (Literal("weave"), Token.Weave),
    (Literal("visitor"), Token.Visitor),
    (Literal("lambda"), Token.Lambda),
    (Literal("and"), Token.And),
    (Literal("or"), Token.Or),
    (Literal("not"), Token.Not),

    (Literal ("new"), Token.New),
    (Literal ("fold"), Token.Fold),
    
    (Literal("into"), Token.Into),
    (Literal("over"), Token.Over),
    (Literal("all"), Token.All),

    (Literal("with"), Token.With),
    (Literal("("), Token.LPar),
    (Literal(")"), Token.RPar),
    (Literal("{"), Token.LBrk),
    (Literal("}"), Token.RBrk),
    (Literal("["), Token.LSBrk),
    (Literal("]"), Token.RSBrk),
    (Literal(","), Token.Comma),
    (Literal(":"), Token.Colon),
    (Literal(";"), Token.Semicolon),
    (Literal("=>"), Token.Arrow),
    (Literal("."), Token.Dot),
    (Literal(":="), Token.Assign),
    (Literal("&"), Token.Amp),
    (Literal("@"), Token.At),

    (Literal("true"), Token.LitTrue),
    (Literal("false"), Token.LitFalse),

    (Pattern('({MLSTRING_SQ}|{MLSTRING_DBQ}'
             '|{STRING_SQ}|{STRING_DBQ})'), Token.String),
    (Pattern(r"([0-9]+)"), Token.Integer),
    (Pattern(r"[a-zA-Z][a-zA-Z0-9_]*"), Token.Identifier),
)
