pub const Token = union(enum) {
    illegal: u8,
    eof: void,

    // an identifier, a literal
    ident: []const u8,
    int: []const u8,
    stringLiteral: []const u8,

    // operators
    assign: void,
    plus: void,
    minus: void,
    bang: void,
    asterisk: void,
    slash: void,
    equal: void,
    notEqual: void,
    lt: void,
    gt: void,

    // delimiters
    comma: void,
    semicolon: void,
    colon: void,
    lparen: void,
    rparen: void,
    lbrace: void,
    rbrace: void,
    lbracket: void,
    rbracket: void,

    // keywords
    function: void,
    let: void,
    true_: void,
    false_: void,
    if_: void,
    else_: void,
    return_: void,
    macro: void,
};
