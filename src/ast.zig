const std = @import("std");
const String = @import("string").String;

pub const Node = union(enum) {
    program: *Program,
    statement: *Statement,
    expression: *Expression,

    pub fn toString(self: Node, buf: *String) !void {
        return switch (self) {
            .program => try self.program.toString(buf),
            .statement => try self.statement.toString(buf),
            .expression => try self.expression.toString(buf),
        };
    }
};

// Root AST
pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn toString(self: Program, buf: *String) !void {
        for (self.statements.items) |statement| {
            try statement.toString(buf);
        }
    }
};

pub const Statement = union(enum) {
    block: Block,
    let: Let,
    return_: Return,
    expressionStatement: ExpressionStatement,

    fn toString(self: Statement, buf: *String) !void {
        switch (self) {
            .block => @panic("unimplments"),
            .let => @panic("unimplments"),
            .return_ => @panic("unimplments"),
            .expressionStatement => |expressionStatement| try expressionStatement.toString(buf),
        }
    }
};
pub const Expression = union(enum) {
    identifier: Identifier,
    prefixExpression: PrefixExpression,
    infixExpression: InfixExpression,
    if_: If,
    function: Function,
    call: Call,
    integer: Integer,
    boolean: Boolean,
    stringLiteral: StringLiteral,
    array: Array,
    index: Index,
    hash: Hash,
    macroLiteral: MacroLiteral,

    fn toString(self: Expression, buf: *String) String.Error!void {
        switch (self) {
            .identifier => |identifier| try identifier.toString(buf),
            .prefixExpression => |prefixExpression| try prefixExpression.toString(buf),
            .infixExpression => |infixExpression| try infixExpression.toString(buf),
            .if_ => |if_| try if_.toString(buf),
            .function => |function| try function.toString(buf),
            .call => |call| try call.toString(buf),
            .integer => |integer| try integer.toString(buf),
            .boolean => |boolean| try boolean.toString(buf),
            .stringLiteral => |stringLiteral| try stringLiteral.toString(buf),
            .array => |array| try array.toString(buf),
            .index => |index| try index.toString(buf),
            .hash => |hash| try hash.toString(buf),
            .macroLiteral => |macroLiteral| try macroLiteral.toString(buf),
        }
    }
};

// Statements
pub const Block = struct {
    statements: std.ArrayList(Statement),

    fn toString(self: Block, buf: *String) String.Error!void {
        try buf.concat("{ ");
        for (self.statements.items) |statement| {
            try statement.toString(buf);
        }
        try buf.concat(" }");
    }
};

pub const Let = struct {
    name: Identifier,
    value: *const Expression,

    fn toString(self: Let, buf: *String) String.Error!void {
        try buf.concat("let ");
        try self.name.toString(buf);
        try self.concat(" = ");
        try self.value.toString(buf);
    }
};

pub const Return = struct {
    value: *const Expression,

    fn toString(self: Return, buf: *String) String.Error!void {
        try buf.concat("return ");
        try self.value.toString(buf);
    }
};

pub const ExpressionStatement = struct {
    expression: *const Expression,

    fn toString(self: ExpressionStatement, buf: *String) String.Error!void {
        try self.expression.toString(buf);
    }
};

// Expressions
pub const Array = struct {
    elements: std.ArrayList(Expression),

    fn toString(self: Array, buf: *String) String.Error!void {
        try buf.concat("[");
        for (self.elements.items) |element, i| {
            try element.toString(buf);
            if (i != self.elements.items.len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat("]");
    }
};
pub const Boolean = struct {
    value: bool,

    fn toString(self: Boolean, buf: *String) String.Error!void {
        if (self.value) {
            try buf.concat("true");
        } else {
            try buf.concat("false");
        }
    }
};
pub const Integer = struct {
    value: i64,

    fn toString(self: Integer, buf: *String) String.Error!void {
        var intString = try std.fmt.allocPrint(buf.allocator.*, "{}", .{self.value});
        try buf.concat(intString);
    }
};
pub const StringLiteral = struct {
    value: []const u8,

    fn toString(self: StringLiteral, buf: *String) String.Error!void {
        try buf.concat("\"");
        try buf.concat(self.value);
        try buf.concat("\"");
    }
};
pub const Function = struct {
    parameters: std.ArrayList(Identifier),
    body: Block,
    name: []const u8,

    fn toString(self: Function, buf: *String) String.Error!void {
        try buf.concat("fn");
        try buf.concat("(");
        for (self.parameters.items) |parameter, i| {
            try parameter.toString(buf);
            if (i != self.parameters.items.len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(") ");
        try self.body.toString(buf);
    }
};
pub const Hash = struct {
    pairs: std.ArrayList(HashPair),

    fn toString(self: Hash, buf: *String) String.Error!void {
        try buf.concat("{");
        for (self.pairs.items) |pair, i| {
            try pair.toString(buf);
            if (i != self.pairs.items.len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat("}");
    }
};

pub const HashPair = struct {
    key: Expression,
    value: Expression,

    fn toString(self: HashPair, buf: *String) String.Error!void {
        try self.key.toString(buf);
        try buf.concat(": ");
        try self.value.toString(buf);
    }
};
pub const Identifier = struct {
    value: []const u8,

    fn toString(self: Identifier, buf: *String) String.Error!void {
        try buf.concat(self.value);
    }
};

pub const If = struct {
    condition: *const Expression,
    thenBranch: Block,
    elseBranch: ?Block,

    fn toString(self: If, buf: *String) String.Error!void {
        try buf.concat("if ");
        try self.condition.toString(buf);
        try buf.concat(" ");
        try self.thenBranch.toString(buf);
        if (self.elseBranch) |elseBranch| {
            try buf.concat(" else ");
            try elseBranch.toString(buf);
        }
    }
};

pub const Call = struct {
    callee: *const Expression,
    arguments: std.ArrayList(Expression),

    fn toString(self: Call, buf: *String) String.Error!void {
        try self.callee.toString(buf);
        try buf.concat("(");
        for (self.arguments.items) |argument, i| {
            try argument.toString(buf);
            if (i != self.arguments.items.len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(")");
    }
};
pub const Index = struct {
    left: *const Expression,
    index: *const Expression,

    fn toString(self: Index, buf: *String) String.Error!void {
        try buf.concat("(");
        try self.left.toString(buf);
        try buf.concat("[");
        try self.index.toString(buf);
        try buf.concat("])");
    }
};

pub const InfixExpression = struct {
    left: *const Expression,
    operator: Operator,
    right: *const Expression,

    fn toString(self: InfixExpression, buf: *String) String.Error!void {
        try buf.concat("(");
        try self.left.toString(buf);
        try buf.concat(" ");
        try buf.concat(self.operator.toString());
        try buf.concat(" ");
        try self.right.toString(buf);
        try buf.concat(")");
    }
};
pub const PrefixExpression = struct {
    operator: Operator,
    right: *const Expression,

    fn toString(self: PrefixExpression, buf: *String) String.Error!void {
        try buf.concat("(");
        try buf.concat(self.operator.toString());
        try self.right.toString(buf);
        try buf.concat(")");
    }
};

pub const MacroLiteral = struct {
    parameters: std.ArrayList(Identifier),
    body: Block,

    fn toString(self: MacroLiteral, buf: *String) String.Error!void {
        try buf.concat("macro(");
        for (self.parameters.items) |parameter, i| {
            try parameter.toString(buf);
            if (i != self.parameters.items.len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(") ");
        try self.body.toString(buf);
    }
};

pub const Operator = enum {
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

    fn toString(self: Operator) []const u8 {
        return switch (self) {
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .bang => "!",
            .asterisk => "*",
            .slash => "/",
            .equal => "==",
            .notEqual => "!=",
            .lt => "<",
            .gt => ">",
        };
    }
};
