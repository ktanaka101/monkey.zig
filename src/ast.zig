const std = @import("std");
const String = @import("string").String;

pub const Node = union(enum) {
    program: *Program,
    statement: *Statement,
    expression: *Expression,

    pub fn toString(self: *Node, buf: *String) !void {
        return switch (self.*) {
            .program => |*program| try program.*.toString(buf),
            .statement => |*statement| try statement.*.toString(buf),
            .expression => |*expression| try expression.*.toString(buf),
        };
    }
};

// Root AST
pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn toString(self: *Program, buf: *String) !void {
        var i: usize = 0;
        while (i < self.statements.items.len) : (i += 1) {
            try self.statements.items[i].toString(buf);
        }
    }
};

pub const Statement = union(enum) {
    block: Block,
    let: Let,
    return_: Return,
    expressionStatement: ExpressionStatement,

    pub fn toString(self: *Statement, buf: *String) String.Error!void {
        switch (self.*) {
            .block => |*block| try block.toString(buf),
            .let => |let| try let.toString(buf),
            .return_ => |return_| try return_.toString(buf),
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

    pub fn toString(self: *Expression, buf: *String) String.Error!void {
        switch (self.*) {
            .identifier => |identifier| try identifier.toString(buf),
            .prefixExpression => |prefixExpression| try prefixExpression.toString(buf),
            .infixExpression => |infixExpression| try infixExpression.toString(buf),
            .if_ => |*if_| try if_.toString(buf),
            .function => |*function| try function.toString(buf),
            .call => |*call| try call.toString(buf),
            .integer => |integer| try integer.toString(buf),
            .boolean => |boolean| try boolean.toString(buf),
            .stringLiteral => |stringLiteral| try stringLiteral.toString(buf),
            .array => |*array| try array.toString(buf),
            .index => |index| try index.toString(buf),
            .hash => |*hash| try hash.toString(buf),
            .macroLiteral => |*macroLiteral| try macroLiteral.toString(buf),
        }
    }
};

// Statements
pub const Block = struct {
    statements: std.ArrayList(Statement),

    pub fn toString(self: *Block, buf: *String) String.Error!void {
        try buf.concat("{ ");
        var i: usize = 0;
        while (i < self.statements.items.len) : (i += 1) {
            try self.statements.items[i].toString(buf);
        }
        try buf.concat(" }");
    }
};

pub const Let = struct {
    name: Identifier,
    value: *Expression,

    pub fn toString(self: Let, buf: *String) String.Error!void {
        try buf.concat("let ");
        try self.name.toString(buf);
        try buf.concat(" = ");
        try self.value.toString(buf);
    }
};

pub const Return = struct {
    value: *Expression,

    pub fn toString(self: Return, buf: *String) String.Error!void {
        try buf.concat("return ");
        try self.value.toString(buf);
    }
};

pub const ExpressionStatement = struct {
    expression: *Expression,

    pub fn toString(self: ExpressionStatement, buf: *String) String.Error!void {
        try self.expression.toString(buf);
    }
};

// Expressions
pub const Array = struct {
    elements: std.ArrayList(Expression),

    pub fn toString(self: *Array, buf: *String) String.Error!void {
        try buf.concat("[");
        const len = self.elements.items.len;
        var i: usize = 0;
        while (i < len) : (i += 1) {
            try self.elements.items[i].toString(buf);
            if (i != len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat("]");
    }
};
pub const Boolean = struct {
    value: bool,

    pub fn toString(self: Boolean, buf: *String) String.Error!void {
        if (self.value) {
            try buf.concat("true");
        } else {
            try buf.concat("false");
        }
    }
};
pub const Integer = struct {
    value: i64,

    pub fn toString(self: Integer, buf: *String) String.Error!void {
        const intString = try std.fmt.allocPrint(buf.allocator, "{}", .{self.value});
        try buf.concat(intString);
    }
};
pub const StringLiteral = struct {
    value: []const u8,

    pub fn toString(self: StringLiteral, buf: *String) String.Error!void {
        try buf.concat("\"");
        try buf.concat(self.value);
        try buf.concat("\"");
    }
};
pub const Function = struct {
    parameters: std.ArrayList(Identifier),
    body: Block,
    name: []const u8,

    pub fn toString(self: *Function, buf: *String) String.Error!void {
        try buf.concat("fn");
        try buf.concat("(");
        const len = self.parameters.items.len;
        var i: usize = 0;
        while (i < len) : (i += 1) {
            try self.parameters.items[i].toString(buf);
            if (i != len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(") ");
        try self.body.toString(buf);
    }
};
pub const Hash = struct {
    pairs: std.ArrayList(HashPair),

    pub fn toString(self: *Hash, buf: *String) String.Error!void {
        try buf.concat("{");
        const len = self.pairs.items.len;
        var i: usize = 0;
        while (i < len) : (i += 1) {
            try self.pairs.items[i].toString(buf);
            if (i != len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat("}");
    }
};

pub const HashPair = struct {
    key: Expression,
    value: Expression,

    pub fn toString(self: *HashPair, buf: *String) String.Error!void {
        try self.key.toString(buf);
        try buf.concat(": ");
        try self.value.toString(buf);
    }
};
pub const Identifier = struct {
    value: []const u8,

    pub fn toString(self: Identifier, buf: *String) String.Error!void {
        try buf.concat(self.value);
    }
};

pub const If = struct {
    condition: *Expression,
    thenBranch: Block,
    elseBranch: ?Block,

    pub fn toString(self: *If, buf: *String) String.Error!void {
        try buf.concat("if ");
        try self.condition.toString(buf);
        try buf.concat(" ");
        try self.thenBranch.toString(buf);
        if (self.elseBranch) |*elseBranch| {
            try buf.concat(" else ");
            try elseBranch.toString(buf);
        }
    }
};

pub const Call = struct {
    callee: *Expression,
    arguments: std.ArrayList(Expression),

    pub fn isQuote(self: Call) bool {
        switch (self.callee.*) {
            .identifier => |identifier| return std.mem.eql(u8, identifier.value, "quote"),
            else => return false,
        }
    }

    pub fn toString(self: *Call, buf: *String) String.Error!void {
        try self.callee.toString(buf);
        try buf.concat("(");
        const len = self.arguments.items.len;
        var i: usize = 0;
        while (i < len) : (i += 1) {
            try self.arguments.items[i].toString(buf);
            if (i != len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(")");
    }
};
pub const Index = struct {
    left: *Expression,
    index: *Expression,

    pub fn toString(self: Index, buf: *String) String.Error!void {
        try buf.concat("(");
        try self.left.toString(buf);
        try buf.concat("[");
        try self.index.toString(buf);
        try buf.concat("])");
    }
};

pub const InfixExpression = struct {
    left: *Expression,
    operator: Operator,
    right: *Expression,

    pub fn toString(self: InfixExpression, buf: *String) String.Error!void {
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
    right: *Expression,

    pub fn toString(self: PrefixExpression, buf: *String) String.Error!void {
        try buf.concat("(");
        try buf.concat(self.operator.toString());
        try self.right.toString(buf);
        try buf.concat(")");
    }
};

pub const MacroLiteral = struct {
    parameters: std.ArrayList(Identifier),
    body: Block,

    pub fn toString(self: *MacroLiteral, buf: *String) String.Error!void {
        try buf.concat("macro(");
        const len = self.parameters.items.len;
        var i: usize = 0;
        while (i < len) : (i += 1) {
            try self.parameters.items[i].toString(buf);
            if (i != len - 1) {
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

    pub fn toString(self: Operator) []const u8 {
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
