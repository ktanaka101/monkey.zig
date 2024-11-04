const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const String = @import("string").String;

const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Lexer = @import("lexer.zig").Lexer;
const ast = @import("ast.zig");

pub const ParserError = error{ ExpectOperator, ExpectExpression, ExpectIdentifier, InvalidPrefix, InvalidInfix, ExpectPeek, InvalidHashLiteral, InvalidStringLiteral, InvalidFunctionParam, InvalidBooleanLiteral, InvalidIntegerLiteral, InvalidInteger, InvalidBlockStatement, InvalidExpressionList, InvalidProgram, MemoryAllocation };

const Priority = enum(u4) {
    lowest = 0,
    equals = 1,
    lessgreater = 2,
    sum = 3,
    product = 4,
    prefix = 5,
    call = 6,
    index = 7,

    fn lessThan(self: Priority, other: Priority) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }

    fn fromToken(token: Token) Priority {
        return switch (token) {
            .equal => .equals,
            .notEqual => .equals,
            .lt => .lessgreater,
            .gt => .lessgreater,
            .plus => .sum,
            .minus => .sum,
            .slash => .product,
            .asterisk => .product,
            .lparen => .call,
            .lbracket => .index,
            else => .lowest,
        };
    }
};

fn getOperatorFromToken(token: Token) !ast.Operator {
    return switch (token) {
        .assign => .assign,
        .plus => .plus,
        .minus => .minus,
        .bang => .bang,
        .asterisk => .asterisk,
        .slash => .slash,
        .equal => .equal,
        .notEqual => .notEqual,
        .lt => .lt,
        .gt => .gt,
        else => ParserError.ExpectOperator,
    };
}

pub const Parser = struct {
    lexer: *Lexer,
    currentToken: Token,
    peekToken: Token,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn new(lexer: *Lexer, allocator: std.mem.Allocator) Self {
        const currentToken = lexer.nextToken();
        const peekToken = lexer.nextToken();

        return .{ .lexer = lexer, .currentToken = currentToken, .peekToken = peekToken, .allocator = allocator };
    }

    fn nextToken(self: *Self) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Self) ParserError!ast.Program {
        var statements = std.ArrayList(ast.Statement).init(self.allocator);

        while (self.currentToken != Token.eof) {
            const statement = try self.parseStatement();
            statements.append(statement) catch return ParserError.InvalidProgram;
            self.nextToken();
        }

        return ast.Program{ .statements = statements };
    }

    fn parseStatement(self: *Self) ParserError!ast.Statement {
        return switch (self.currentToken) {
            .let => ast.Statement{ .let = try self.parseLetStatement() },
            .return_ => ast.Statement{ .return_ = try self.parseReturnStatement() },
            else => ast.Statement{ .expressionStatement = try self.parseExpressionStatement() },
        };
    }

    fn parseLetStatement(self: *Self) ParserError!ast.Let {
        try self.expectPeek(.ident);

        const name =
            switch (self.currentToken) {
            .ident => |ident| ast.Identifier{ .value = ident },
            else => unreachable,
        };

        try self.expectPeek(.assign);
        self.nextToken();

        var expression = try self.parseExpression(.lowest);
        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        switch (expression) {
            .function => |*function| {
                function.*.name = name.value;
            },
            else => {},
        }

        const expressionPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        expressionPtr.* = expression;
        return ast.Let{ .name = name, .value = expressionPtr };
    }

    fn parseReturnStatement(self: *Self) ParserError!ast.Return {
        self.nextToken();

        const returnValue = try self.parseExpression(.lowest);
        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        const returnValuePtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        returnValuePtr.* = returnValue;
        return ast.Return{ .value = returnValuePtr };
    }

    fn parseBlockStatement(self: *Self) ParserError!ast.Block {
        var statements = std.ArrayList(ast.Statement).init(self.allocator);

        self.nextToken();

        while (!self.currentTokenIs(.rbrace) and !self.currentTokenIs(.eof)) {
            const statement = try self.parseStatement();
            statements.append(statement) catch return ParserError.InvalidBlockStatement;
            self.nextToken();
        }

        return ast.Block{ .statements = statements };
    }

    fn parseExpressionStatement(self: *Self) ParserError!ast.ExpressionStatement {
        const expression = try self.parseExpression(.lowest);
        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        const expressionPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        expressionPtr.* = expression;
        return ast.ExpressionStatement{ .expression = expressionPtr };
    }

    fn parseExpression(self: *Self, precedende: Priority) ParserError!ast.Expression {
        var leftExpression = try self.parseExpressionByPrefixToken(self.currentToken);

        while (!self.peekTokenIs(.semicolon) and precedende.lessThan(Priority.fromToken(self.peekToken))) {
            const leftExpressionPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
            leftExpressionPtr.* = leftExpression;

            leftExpression = try self.parseInfixExpressionByToken(self.peekToken, leftExpressionPtr);
        }

        return leftExpression;
    }

    fn parseIdentifier(self: Self) ParserError!ast.Identifier {
        return switch (self.currentToken) {
            .ident => |value| ast.Identifier{ .value = value },
            else => ParserError.ExpectIdentifier,
        };
    }

    fn parseInteger(self: Self) ParserError!ast.Integer {
        return switch (self.currentToken) {
            .int => |value| ast.Integer{ .value = std.fmt.parseInt(i64, value, 10) catch return ParserError.InvalidInteger },
            else => ParserError.InvalidIntegerLiteral,
        };
    }

    fn parseBoolean(self: Self) ParserError!ast.Boolean {
        return switch (self.currentToken) {
            .true_ => ast.Boolean{ .value = true },
            .false_ => ast.Boolean{ .value = false },
            else => ParserError.InvalidBooleanLiteral,
        };
    }

    fn parseGroupedExpression(self: *Self) ParserError!ast.Expression {
        self.nextToken();
        const expression = try self.parseExpression(.lowest);
        try self.expectPeek(.rparen);
        return expression;
    }

    fn parsePrefixExpression(self: *Self) ParserError!ast.PrefixExpression {
        const operator = try getOperatorFromToken(self.currentToken);
        self.nextToken();
        const right = try self.parseExpression(.prefix);
        const rightPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        rightPtr.* = right;

        return ast.PrefixExpression{ .operator = operator, .right = rightPtr };
    }

    fn parseInfixExpression(self: *Self, left: *ast.Expression) ParserError!ast.InfixExpression {
        const operator = try getOperatorFromToken(self.currentToken);
        const priority = Priority.fromToken(self.currentToken);

        self.nextToken();

        const right = try self.parseExpression(priority);
        const rightPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        rightPtr.* = right;

        return ast.InfixExpression{ .operator = operator, .left = left, .right = rightPtr };
    }

    fn parseIfExpression(self: *Self) ParserError!ast.If {
        try self.expectPeek(.lparen);

        self.nextToken();

        const condition = try self.parseExpression(.lowest);
        const conditionPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        conditionPtr.* = condition;

        try self.expectPeek(.rparen);
        try self.expectPeek(.lbrace);

        const thenBlock = try self.parseBlockStatement();
        var elseBlock: ?ast.Block = null;
        if (self.peekTokenIs(.else_)) {
            self.nextToken();
            try self.expectPeek(.lbrace);

            elseBlock = try self.parseBlockStatement();
        }

        return ast.If{ .condition = conditionPtr, .thenBranch = thenBlock, .elseBranch = elseBlock };
    }

    fn parseFunctionLiteral(self: *Self) ParserError!ast.Function {
        try self.expectPeek(.lparen);

        const parameters = try self.parseFunctionParameters();
        try self.expectPeek(.lbrace);

        const body = try self.parseBlockStatement();

        return ast.Function{ .parameters = parameters, .body = body, .name = "" };
    }

    fn parseFunctionParameters(self: *Self) ParserError!std.ArrayList(ast.Identifier) {
        var parameters = std.ArrayList(ast.Identifier).init(self.allocator);
        if (self.peekTokenIs(.rparen)) {
            self.nextToken();
            return parameters;
        }

        self.nextToken();

        parameters.append(try self.parseIdentifier()) catch return ParserError.InvalidFunctionParam;

        while (self.peekTokenIs(.comma)) {
            self.nextToken();
            self.nextToken();
            parameters.append(try self.parseIdentifier()) catch return ParserError.InvalidFunctionParam;
        }
        try self.expectPeek(.rparen);

        return parameters;
    }

    fn parseCallExpression(self: *Self, callee: *ast.Expression) ParserError!ast.Call {
        return ast.Call{ .callee = callee, .arguments = try self.parseExpressionList(.rparen) };
    }

    fn parseExpressionList(self: *Self, endToken: TokenTag) ParserError!std.ArrayList(ast.Expression) {
        var list = std.ArrayList(ast.Expression).init(self.allocator);
        if (self.peekTokenIs(endToken)) {
            self.nextToken();
            return list;
        }

        self.nextToken();
        list.append(try self.parseExpression(.lowest)) catch return ParserError.InvalidExpressionList;

        while (self.peekTokenIs(.comma)) {
            self.nextToken();
            self.nextToken();
            list.append(try self.parseExpression(.lowest)) catch return ParserError.InvalidExpressionList;
        }
        try self.expectPeek(endToken);

        return list;
    }

    fn parseStringLiteral(self: Self) ParserError!ast.StringLiteral {
        return switch (self.currentToken) {
            .stringLiteral => |value| ast.StringLiteral{ .value = value },
            else => ParserError.InvalidStringLiteral,
        };
    }

    fn parseArrayLiteral(self: *Self) ParserError!ast.Array {
        return ast.Array{ .elements = try self.parseExpressionList(.rbracket) };
    }

    fn parseIndexExpression(self: *Self, left: *ast.Expression) ParserError!ast.Index {
        self.nextToken();
        const index = try self.parseExpression(.lowest);
        const indexPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        indexPtr.* = index;

        try self.expectPeek(.rbracket);

        return ast.Index{ .left = left, .index = indexPtr };
    }

    fn parseHashLiteral(self: *Self) ParserError!ast.Hash {
        var hash = ast.Hash{ .pairs = std.ArrayList(ast.HashPair).init(self.allocator) };

        while (!self.peekTokenIs(.rbrace)) {
            self.nextToken();
            const key = try self.parseExpression(.lowest);
            try self.expectPeek(.colon);
            self.nextToken();
            const value = try self.parseExpression(.lowest);
            hash.pairs.append(ast.HashPair{ .key = key, .value = value }) catch return ParserError.InvalidHashLiteral;

            if (!self.peekTokenIs(.rbrace)) {
                self.expectPeek(.comma) catch return ParserError.InvalidHashLiteral;
            }
        }
        try self.expectPeek(.rbrace);

        return hash;
    }

    fn parseMacroLiteral(self: *Self) ParserError!ast.MacroLiteral {
        try self.expectPeek(.lparen);
        const parameters = try self.parseFunctionParameters();

        try self.expectPeek(.lbrace);
        const body = try self.parseBlockStatement();

        return ast.MacroLiteral{ .parameters = parameters, .body = body };
    }

    fn currentTokenIs(self: Self, token: TokenTag) bool {
        return self.currentToken == token;
    }

    fn peekTokenIs(self: Self, token: TokenTag) bool {
        return self.peekToken == token;
    }

    fn expectPeek(self: *Self, token: TokenTag) ParserError!void {
        if (self.peekTokenIs(token)) {
            self.nextToken();
        } else {
            return ParserError.ExpectPeek;
        }
    }

    fn parseExpressionByPrefixToken(self: *Self, token: TokenTag) ParserError!ast.Expression {
        return switch (token) {
            .ident => ast.Expression{ .identifier = try self.parseIdentifier() },
            .int => ast.Expression{ .integer = try self.parseInteger() },
            .bang, .minus => ast.Expression{ .prefixExpression = try self.parsePrefixExpression() },
            .true_, .false_ => ast.Expression{ .boolean = try self.parseBoolean() },
            .lparen => try self.parseGroupedExpression(),
            .if_ => ast.Expression{ .if_ = try self.parseIfExpression() },
            .function => ast.Expression{ .function = try self.parseFunctionLiteral() },
            .stringLiteral => ast.Expression{ .stringLiteral = try self.parseStringLiteral() },
            .lbracket => ast.Expression{ .array = try self.parseArrayLiteral() },
            .lbrace => ast.Expression{ .hash = try self.parseHashLiteral() },
            .macro => ast.Expression{ .macroLiteral = try self.parseMacroLiteral() },
            else => ParserError.InvalidPrefix,
        };
    }

    fn parseInfixExpressionByToken(self: *Self, token: TokenTag, left: *ast.Expression) ParserError!ast.Expression {
        self.nextToken();
        return switch (token) {
            .plus, .minus, .asterisk, .slash, .equal, .notEqual, .gt, .lt => ast.Expression{ .infixExpression = try self.parseInfixExpression(left) },
            .lparen => ast.Expression{ .call = try self.parseCallExpression(left) },
            .lbracket => ast.Expression{ .index = try self.parseIndexExpression(left) },
            else => ParserError.InvalidInfix,
        };
    }
};

fn parseProgramForTesting(actualInput: []const u8, expecting: fn (*const ast.Program) anyerror!void) !void {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();

    var lexer = Lexer.new(actualInput);
    var parser = Parser.new(&lexer, allocator.allocator());
    const program = try parser.parseProgram();

    try expecting(&program);
}

test "Parser.new" {
    var lexer = Lexer.new(":;");
    const parser = Parser.new(&lexer, std.testing.allocator);
    try expectEqual(Token.colon, parser.currentToken);
    try expectEqual(Token.semicolon, parser.peekToken);
}

fn expectLetStatement(expected: *const ast.Let, actual: *const ast.Let) !void {
    try expectIdentifier(&expected.*.name, &actual.*.name);
    try expectExpression(expected.*.value, actual.*.value);
}

fn expectLetStatementByStatement(expected: *const ast.Let, actual: *const ast.Statement) !void {
    switch (actual.*) {
        .let => |let| try expectLetStatement(expected, &let),
        else => {
            std.debug.print("expected .let, found {}\n", .{actual});
            return error.TestExpectedLetStatementByStatement;
        },
    }
}

fn expectReturnStatement(expected: *const ast.Return, actual: *const ast.Return) !void {
    try expectExpression(expected.*.value, actual.*.value);
}

fn expectReturnStatementByStatement(expected: *const ast.Return, actual: *const ast.Statement) !void {
    switch (actual.*) {
        .return_ => |return_| try expectReturnStatement(expected, &return_),
        else => {
            std.debug.print("expected .return_, found {}\n", .{actual});
            return error.TestExpectedReturnStatementByStatement;
        },
    }
}

fn expectExpressionStatement(expected: *const ast.ExpressionStatement, actual: *const ast.ExpressionStatement) !void {
    try expectExpression(expected.*.expression, actual.*.expression);
}

fn expectExpressionStatementByStatement(expected: *const ast.ExpressionStatement, actual: *const ast.Statement) !void {
    switch (actual.*) {
        .expressionStatement => |expressionStatement| try expectExpressionStatement(expected, &expressionStatement),
        else => {
            std.debug.print("expected .return_, found {}\n", .{actual});
            return error.TestExpectedExpressionStatementByStatement;
        },
    }
}

fn expectStatement(expected: *const ast.Statement, actual: *const ast.Statement) !void {
    switch (expected.*) {
        .let => |let| try expectLetStatementByStatement(&let, actual),
        .return_ => |return_| try expectReturnStatementByStatement(&return_, actual),
        .expressionStatement => |expressionStatement| try expectExpressionStatementByStatement(&expressionStatement, actual),
        else => {
            std.debug.print("unsupported {}\n", .{expected});
            return error.TestExpectedStatement;
        },
    }
}

fn expectIdentifier(expected: *const ast.Identifier, actual: *const ast.Identifier) !void {
    try expectEqualStrings(expected.*.value, actual.*.value);
}

fn expectInteger(expected: *const ast.Integer, actual: *const ast.Integer) !void {
    try expectEqual(expected.*.value, actual.*.value);
}

fn expectBoolean(expected: *const ast.Boolean, actual: *const ast.Boolean) !void {
    try expectEqual(expected.*.value, actual.*.value);
}

fn expectPrefixExpression(expected: *const ast.PrefixExpression, actual: *const ast.PrefixExpression) anyerror!void {
    try expectEqual(expected.*.operator, actual.*.operator);
    try expectExpression(expected.*.right, actual.*.right);
}

fn expectInfixExpression(expected: *const ast.InfixExpression, actual: *const ast.InfixExpression) anyerror!void {
    try expectExpression(expected.*.left, actual.*.left);
    try expectEqual(expected.*.operator, actual.*.operator);
    try expectExpression(expected.*.right, actual.*.right);
}

fn expectExpression(expected: *const ast.Expression, actual: *const ast.Expression) !void {
    switch (expected.*) {
        .integer => {
            switch (actual.*) {
                .integer => |integer| try expectInteger(&expected.*.integer, &integer),
                else => {
                    std.debug.print("expected .integer, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },
        .boolean => {
            switch (actual.*) {
                .boolean => |boolean| try expectBoolean(&expected.*.boolean, &boolean),
                else => {
                    std.debug.print("expected .boolean, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },
        .identifier => {
            switch (actual.*) {
                .identifier => |identifier| try expectIdentifier(&expected.*.identifier, &identifier),
                else => {
                    std.debug.print("expected .identifier, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },
        .prefixExpression => {
            switch (actual.*) {
                .prefixExpression => |prefixExpression| try expectPrefixExpression(&expected.*.prefixExpression, &prefixExpression),
                else => {
                    std.debug.print("expected .prefixExpression, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },
        .infixExpression => {
            switch (actual.*) {
                .infixExpression => |infixExpression| try expectInfixExpression(&expected.*.infixExpression, &infixExpression),
                else => {
                    std.debug.print("expected .infixExpression, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },
        else => {
            std.debug.print("unsupported {}\n", .{expected});
            return error.TestExpectedExpression;
        },
    }
}

fn expectOneStatementInProgram(
    expected: *const ast.Statement,
    actual: *const ast.Program,
) !void {
    try expectEqual(@as(usize, 1), actual.*.statements.items.len);
    try expectStatement(expected, &actual.*.statements.items[0]);
}

fn expectEqualStringParsedProgram(expected: []const u8, actual: []const u8) !void {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();

    var lexer = Lexer.new(actual);
    var parser = Parser.new(&lexer, allocator.allocator());
    var program = try parser.parseProgram();

    var buf = String.init(allocator.allocator());
    try program.toString(&buf);
    const ok = buf.cmp(expected);
    if (!ok) {
        std.debug.print("\n\texpected: {s} \n\t   found: {s}\n", .{ expected, buf.str() });
        return error.TestExpectEqualStringParsedProgram;
    }
}

test "let statements" {
    {
        try parseProgramForTesting("let x = 5", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
                try expectOneStatementInProgram(&ast.Statement{ .let = ast.Let{
                    .name = ast.Identifier{ .value = "x" },
                    .value = &integer,
                } }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("let x = 5;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
                try expectOneStatementInProgram(&ast.Statement{ .let = ast.Let{ .name = ast.Identifier{ .value = "x" }, .value = &integer } }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("let y = true;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean = ast.Expression{ .boolean = ast.Boolean{ .value = true } };
                try expectOneStatementInProgram(&ast.Statement{ .let = ast.Let{
                    .name = ast.Identifier{ .value = "y" },
                    .value = &boolean,
                } }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("let foobar = y;", struct {
            fn function(program: *const ast.Program) !void {
                var id = ast.Expression{ .identifier = ast.Identifier{ .value = "y" } };
                try expectOneStatementInProgram(&ast.Statement{ .let = ast.Let{
                    .name = ast.Identifier{ .value = "foobar" },
                    .value = &id,
                } }, program);
            }
        }.function);
    }
}

test {
    {
        try parseProgramForTesting("return 5", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
                try expectOneStatementInProgram(&ast.Statement{ .return_ = ast.Return{
                    .value = &integer,
                } }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("return 5;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
                try expectOneStatementInProgram(&ast.Statement{ .return_ = ast.Return{
                    .value = &integer,
                } }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("return true;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean = ast.Expression{ .boolean = ast.Boolean{ .value = true } };
                try expectOneStatementInProgram(&ast.Statement{ .return_ = ast.Return{
                    .value = &boolean,
                } }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("return y;", struct {
            fn function(program: *const ast.Program) !void {
                var id = ast.Expression{ .identifier = ast.Identifier{ .value = "y" } };
                try expectOneStatementInProgram(&ast.Statement{ .return_ = ast.Return{
                    .value = &id,
                } }, program);
            }
        }.function);
    }
}

test "identifier expression" {
    try parseProgramForTesting("foobar", struct {
        fn function(program: *const ast.Program) !void {
            var id = ast.Expression{ .identifier = ast.Identifier{ .value = "foobar" } };
            try expectOneStatementInProgram(&ast.Statement{
                .expressionStatement = ast.ExpressionStatement{ .expression = &id },
            }, program);
        }
    }.function);
}

test "integer expression" {
    {
        try parseProgramForTesting("5", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &integer },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &integer },
                }, program);
            }
        }.function);
    }
}

test "boolean expression" {
    {
        try parseProgramForTesting("true", struct {
            fn function(program: *const ast.Program) !void {
                var boolean = ast.Expression{ .boolean = ast.Boolean{ .value = true } };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &boolean },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("true;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean = ast.Expression{ .boolean = ast.Boolean{ .value = true } };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &boolean },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("false;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean = ast.Expression{ .boolean = ast.Boolean{ .value = false } };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &boolean },
                }, program);
            }
        }.function);
    }
}

test "prefix expression" {
    {
        try parseProgramForTesting("!5", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var prefix = ast.Expression{
                    .prefixExpression = ast.PrefixExpression{ .operator = ast.Operator.bang, .right = &integer },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &prefix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("!5;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var prefix = ast.Expression{
                    .prefixExpression = ast.PrefixExpression{ .operator = ast.Operator.bang, .right = &integer },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &prefix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("-15;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{
                    .integer = ast.Integer{
                        .value = 15,
                    },
                };
                var prefix = ast.Expression{
                    .prefixExpression = ast.PrefixExpression{ .operator = ast.Operator.minus, .right = &integer },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &prefix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("!true;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean = ast.Expression{
                    .boolean = ast.Boolean{
                        .value = true,
                    },
                };
                var prefix = ast.Expression{
                    .prefixExpression = ast.PrefixExpression{ .operator = ast.Operator.bang, .right = &boolean },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &prefix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("!false;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean = ast.Expression{
                    .boolean = ast.Boolean{
                        .value = false,
                    },
                };
                var prefix = ast.Expression{
                    .prefixExpression = ast.PrefixExpression{ .operator = ast.Operator.bang, .right = &boolean },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &prefix },
                }, program);
            }
        }.function);
    }
}

test "infix expression" {
    {
        try parseProgramForTesting("5 + 10", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{ .left = &integer1, .operator = ast.Operator.plus, .right = &integer2 },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5 + 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{ .left = &integer1, .operator = ast.Operator.plus, .right = &integer2 },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5 - 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &integer1,
                        .operator = ast.Operator.minus,
                        .right = &integer2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5 * 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &integer1,
                        .operator = ast.Operator.asterisk,
                        .right = &integer2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5 / 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &integer1,
                        .operator = ast.Operator.slash,
                        .right = &integer2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5 > 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{ .left = &integer1, .operator = ast.Operator.gt, .right = &integer2 },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5 < 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &integer1,
                        .operator = ast.Operator.lt,
                        .right = &integer2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5 == 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &integer1,
                        .operator = ast.Operator.equal,
                        .right = &integer2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("5 != 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer1 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 5,
                    },
                };
                var integer2 = ast.Expression{
                    .integer = ast.Integer{
                        .value = 10,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &integer1,
                        .operator = ast.Operator.notEqual,
                        .right = &integer2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("true == true;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean1 = ast.Expression{
                    .boolean = ast.Boolean{
                        .value = true,
                    },
                };
                var boolean2 = ast.Expression{
                    .boolean = ast.Boolean{
                        .value = true,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &boolean1,
                        .operator = ast.Operator.equal,
                        .right = &boolean2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("true != false;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean1 = ast.Expression{
                    .boolean = ast.Boolean{
                        .value = true,
                    },
                };
                var boolean2 = ast.Expression{
                    .boolean = ast.Boolean{
                        .value = false,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &boolean1,
                        .operator = ast.Operator.notEqual,
                        .right = &boolean2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("false == false;", struct {
            fn function(program: *const ast.Program) !void {
                var boolean1 = ast.Expression{
                    .boolean = ast.Boolean{
                        .value = false,
                    },
                };
                var boolean2 = ast.Expression{
                    .boolean = ast.Boolean{
                        .value = false,
                    },
                };
                var infix = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &boolean1,
                        .operator = ast.Operator.equal,
                        .right = &boolean2,
                    },
                };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &infix },
                }, program);
            }
        }.function);
    }
}

test "operator precedence parsing" {
    try expectEqualStringParsedProgram(
        "((-a) * b)",
        "-a * b",
    );
    try expectEqualStringParsedProgram(
        "(!(-a))",
        "!-a",
    );
    try expectEqualStringParsedProgram(
        "((a + b) + c)",
        "a + b + c",
    );
    try expectEqualStringParsedProgram(
        "((a + b) - c)",
        "a + b - c",
    );
    try expectEqualStringParsedProgram(
        "((a * b) * c)",
        "a * b * c",
    );
    try expectEqualStringParsedProgram(
        "((a * b) / c)",
        "a * b / c",
    );
    try expectEqualStringParsedProgram(
        "(a + (b / c))",
        "a + b / c",
    );
    try expectEqualStringParsedProgram(
        "(((a + (b * c)) + (d / e)) - f)",
        "a + b * c + d / e - f",
    );
    try expectEqualStringParsedProgram(
        "(3 + 4)((-5) * 5)",
        "3 + 4; -5 * 5",
    );
    try expectEqualStringParsedProgram(
        "((5 > 4) == (3 < 4))",
        "5 > 4 == 3 < 4",
    );
    try expectEqualStringParsedProgram(
        "((5 < 4) != (3 > 4))",
        "5 < 4 != 3 > 4",
    );
    try expectEqualStringParsedProgram(
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
    );
    try expectEqualStringParsedProgram(
        "true",
        "true",
    );
    try expectEqualStringParsedProgram(
        "false",
        "false",
    );
    try expectEqualStringParsedProgram(
        "((3 > 5) == false)",
        "3 > 5 == false",
    );
    try expectEqualStringParsedProgram(
        "((3 < 5) == true)",
        "3 < 5 == true",
    );
    try expectEqualStringParsedProgram(
        "((1 + (2 + 3)) + 4)",
        "1 + (2 + 3) + 4",
    );
    try expectEqualStringParsedProgram(
        "((5 + 5) * 2)",
        "(5 + 5) * 2",
    );
    try expectEqualStringParsedProgram(
        "(2 / (5 + 5))",
        "2 / (5 + 5)",
    );
    try expectEqualStringParsedProgram(
        "(-(5 + 5))",
        "-(5 + 5)",
    );
    try expectEqualStringParsedProgram(
        "(!(true == true))",
        "!(true == true)",
    );
    try expectEqualStringParsedProgram(
        "((a + add((b * c))) + d)",
        "a + add(b * c) + d",
    );
    try expectEqualStringParsedProgram(
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
    );
    try expectEqualStringParsedProgram(
        "add((((a + b) + ((c * d) / f)) + g))",
        "add(a + b + c * d / f  + g)",
    );
    try expectEqualStringParsedProgram(
        "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        "a * [1, 2, 3, 4][b * c] * d",
    );
    try expectEqualStringParsedProgram(
        "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        "add(a * b[2], b[1], 2 * [1, 2][1])",
    );
}

test "if expressions" {
    try expectEqualStringParsedProgram(
        "if (x < y) { x }",
        "if (x < y) { x }",
    );
    try expectEqualStringParsedProgram(
        "if (x < y) { x }",
        "if (x < y) { x; }",
    );
    try expectEqualStringParsedProgram(
        "if (x < y) { x } else { y }",
        "if (x < y) { x } else { y }",
    );
    try expectEqualStringParsedProgram(
        "if (x < y) { x } else { y }",
        "if (x < y) { x; } else { y; }",
    );
}

test "function expressions" {
    try expectEqualStringParsedProgram(
        "fn() { 10 }",
        "fn() { 10 }",
    );
    try expectEqualStringParsedProgram(
        "fn() { 10 }",
        "fn() { 10; }",
    );

    try expectEqualStringParsedProgram(
        "fn(x, y) { (x + y) }",
        "fn(x, y) { x + y }",
    );
    try expectEqualStringParsedProgram(
        "fn(x, y) { (x + y) }",
        "fn(x, y) { x + y; }",
    );
}

test "function parameters" {
    try expectEqualStringParsedProgram(
        "fn() {  }",
        "fn() {}",
    );
    try expectEqualStringParsedProgram(
        "fn(x) {  }",
        "fn(x) {}",
    );
    try expectEqualStringParsedProgram(
        "fn(x, y, z) {  }",
        "fn(x, y, z) {}",
    );
}

test "function literal with name" {
    try parseProgramForTesting("let my_function = fn() {}", struct {
        fn function(program: *const ast.Program) !void {
            try expectEqual(@as(usize, 1), program.*.statements.items.len);
            switch (program.*.statements.items[0]) {
                .let => |let| {
                    switch (let.value.*) {
                        .function => |func| {
                            try expectEqualStrings("my_function", func.name);
                        },
                        else => |expression| std.debug.panic("expected let, actual: {}", .{expression}),
                    }
                },
                else => |statement| std.debug.panic("expected let, actual: {}", .{statement}),
            }
        }
    }.function);
}

test "call expressions" {
    try expectEqualStringParsedProgram(
        "add(1, (2 * 3), (4 + 5))",
        "add(1, 2 * 3, 4 + 5);",
    );
}

test "string expressions" {
    try expectEqualStringParsedProgram(
        "\"Hello\"",
        "\"Hello\"",
    );
    try expectEqualStringParsedProgram(
        "\"Hello\"",
        "\"Hello\";",
    );
}

test "array expressions" {
    try expectEqualStringParsedProgram(
        "[]",
        "[];",
    );
    try expectEqualStringParsedProgram(
        "[]",
        "[]",
    );
    try expectEqualStringParsedProgram(
        "[1]",
        "[1];",
    );
    try expectEqualStringParsedProgram(
        "[1, (2 * 3), (4 + 5)]",
        "[1, 2 * 3, 4 + 5];",
    );
}

test "index expressions" {
    try expectEqualStringParsedProgram(
        "(myArray[1])",
        "myArray[1];",
    );
    try expectEqualStringParsedProgram(
        "(myArray[1])",
        "myArray[1]",
    );

    try expectEqualStringParsedProgram(
        "([1, 2, 3][1])",
        "[1, 2, 3][1]",
    );

    try expectEqualStringParsedProgram(
        "([1, 2, 3][(1 + 1)])",
        "[1, 2, 3][1 + 1]",
    );
}

test "hash expressions" {
    try expectEqualStringParsedProgram(
        "{}",
        "{};",
    );
    try expectEqualStringParsedProgram(
        "{}",
        "{}",
    );

    try expectEqualStringParsedProgram(
        "{\"one\": 1}",
        "{\"one\": 1}",
    );

    try expectEqualStringParsedProgram(
        "{\"one\": 1, \"two\": 2, \"three\": 3}",
        "{\"one\": 1, \"two\": 2, \"three\": 3}",
    );

    try expectEqualStringParsedProgram(
        "{\"one\": (0 + 1), \"two\": (10 - 8), \"three\": (15 / 5)}",
        "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}",
    );

    try expectEqualStringParsedProgram(
        "{1: 111, 2: \"b\", 3: true}",
        "{1: 111, 2: \"b\", 3: true}",
    );

    try expectEqualStringParsedProgram(
        "{true: 1, false: \"abc\"}",
        "{true: 1, false: \"abc\"}",
    );
}

test "macro literal parsing" {
    try expectEqualStringParsedProgram(
        "macro(x, y) { (x + y) }",
        "macro(x, y) { x + y }",
    );
    try expectEqualStringParsedProgram(
        "macro(x, y) { (x + y) }",
        "macro(x, y) { x + y; }",
    );
}
