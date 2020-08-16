from langkit.lexer import Lexer, LexerToken, Literal, WithText, Pattern, Ignore, WithSymbol, WithTrivia


class Token(LexerToken):
    Module     = WithText()
    Import     = WithText()
    
    Template   = WithText()
    Extends    = WithText()
    Var        = WithText()

    Match      = WithText()
    Elsmatch   = WithText()
    Defer      = WithText ()
    Do         = WithText()
    Then       = WithText()
    Else       = WithText()
    End        = WithText()
    Weave      = WithText()
    Wrap       = WithText()    
    Pick       = WithText()
    
    Function   = WithText()
    Lambda     = WithText()
    
    And        = WithText()
    Or         = WithText()
    Not        = WithText()
    
    New        = WithText()
    Fold       = WithText()

    Into       = WithText()
    Over       = WithText()
    All        = WithText()
    Is         = WithText()
    Has        = WithText()
    Many       = WithText()
    Few        = WithText()

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
    Backslash  = WithText()
    Plus       = WithText()
    Minus      = WithText()
    Multiply   = WithText()
    Divide     = WithText()

    String     = WithText()
    Comment    = WithTrivia()
    Integer    = WithText()
    Identifier = WithSymbol()

    LitTrue   = WithText()
    LitFalse   = WithText()

template_lexer = Lexer(Token)

template_lexer.add_patterns(
    ("STRING_DBQ", r'(i|s|x|r|)\"(\\\"|[^\n\"])*\"'),
    ("MLSTRING_DBQ", r'(i|s|x|r|)\"\"\"([^"]|("[^"])|(""[^"])|\n)*\"\"\"')
)

template_lexer.add_rules(
    (Pattern(r"[ \t\r\n]+"), Ignore()),
    (Pattern(r"#(.?)+"),     Token.Comment),

    (Literal("import"), Token.Import),
    
    (Literal ("template"), Token.Template),
    (Literal ("extends"), Token.Extends),
    (Literal ("var"), Token.Var),

    (Literal("match"), Token.Match),
    (Literal("elsmatch"), Token.Elsmatch),
    (Literal("defer"), Token.Defer),
    (Literal("do"), Token.Do),
    (Literal("then"), Token.Then),
    (Literal("else"), Token.Else),
    (Literal("end"), Token.End),
    (Literal("wrap"), Token.Wrap),
    (Literal("weave"), Token.Weave),
    (Literal("function"), Token.Function),
    (Literal("lambda"), Token.Lambda),
    (Literal("and"), Token.And),
    (Literal("or"), Token.Or),
    (Literal("not"), Token.Not),

    (Literal ("new"), Token.New),
    (Literal ("fold"), Token.Fold),
    
    (Literal("into"), Token.Into),
    (Literal("over"), Token.Over),
    (Literal("all"), Token.All),
    (Literal("is"), Token.Is),
    (Literal("has"), Token.Has),
    (Literal("many"), Token.Many),
    (Literal("few"), Token.Few),
    
    (Literal("pick"), Token.Pick),
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
    (Literal("\\"), Token.Backslash),

    (Literal("+"), Token.Plus),
    (Literal("-"), Token.Minus),
    (Literal("*"), Token.Multiply),
    (Literal("/"), Token.Divide),

    (Literal("true"), Token.LitTrue),
    (Literal("false"), Token.LitFalse),

    (Pattern('({MLSTRING_DBQ}|{STRING_DBQ})'), Token.String),
    (Pattern(r"([0-9]+)"), Token.Integer),
    (Pattern(r"[a-zA-Z][a-zA-Z0-9_]*"), Token.Identifier),
)
