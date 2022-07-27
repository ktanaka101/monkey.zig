// Root AST
pub const Program = struct { statements: []Statement };

pub const Statement = union {
    block: Block,
    let: Let,
    return_: Return,
    expressionStatement: ExpressionStatement,
};
pub const Expression = union { identifier: Identifier, prefixExpr: PrefixExpression, infixExpression: InfixExpression, if_: If, function: Function, call: Call, integer: Integer, boolean: Boolean, stringLiteral: StringLiteral, array: Array, index: Index, hash: Hash, macroLiteral: MacroLiteral };

// Statements
pub const Block = struct { statements: []Statement };

pub const Let = struct {
    name: Identifier,
    value: Expression,
};

pub const Return = struct {
    value: Expression,
};

pub const ExpressionStatement = struct {
    expression: Expression,
};

// Expressions
pub const Array = struct { elements: []Expression };
pub const Boolean = struct { value: bool };
pub const Integer = struct { value: i64 };
pub const StringLiteral = struct { value: []u8 };
pub const Function = struct { parameters: []Identifier, body: Block, name: []const u8 };
pub const Hash = struct {
    pairs: []HashPair,
};

pub const HashPair = struct { key: Expression, value: Expression };
pub const Identifier = struct { value: []const u8 };

pub const If = struct {
    condition: Expression,
    thenBranch: Block,
    elseBranch: ?Block,
};

pub const Call = struct {
    callee: Expression,
    arguments: []Expression,
};
pub const Index = struct {
    left: Expression,
    index: Expression,
};

pub const InfixExpression = struct {
    left: Expression,
    operator: Operator,
    right: Expression,
};
pub const PrefixExpression = struct {
    operator: Operator,
    right: Expression,
};

pub const MacroLiteral = struct { parameters: []Identifier, body: Block };

pub const Operator = union {
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    equal,
    notEqual,
    lt,
    gt,
};
