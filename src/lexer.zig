const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const token = @import("token.zig");

/// Support only ASCII-Charcter
pub const Lexer = struct {
    input: []const u8,
    position: usize,
    readPosition: usize,
    char: ?u8,

    const Self = @This();

    pub fn new(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
            .position = 0,
            .readPosition = 0,
            .char = null,
        };
        lexer.readChar();

        return lexer;
    }

    pub fn nextToken(self: *Self) token.Token {
        self.skipWhitespace();

        const parsedToken = self.resolveNextToken();

        return parsedToken;
    }

    fn resolveNextToken(self: *Self) token.Token {
        if (self.charIs('=')) {
            if (self.peekCharIs('=')) {
                self.readChar();
                self.readChar();

                return token.Token.equal;
            }
            self.readChar();
            return token.Token.assign;
        } else if (self.charIs('+')) {
            self.readChar();
            return token.Token.plus;
        } else if (self.charIs('-')) {
            self.readChar();
            return token.Token.minus;
        } else if (self.charIs('!')) {
            if (self.peekCharIs('=')) {
                self.readChar();
                self.readChar();
                return token.Token.notEqual;
            }
            self.readChar();
            return token.Token.bang;
        } else if (self.charIs('*')) {
            self.readChar();
            return token.Token.asterisk;
        } else if (self.charIs('/')) {
            self.readChar();
            return token.Token.slash;
        } else if (self.charIs('<')) {
            self.readChar();
            return token.Token.lt;
        } else if (self.charIs('>')) {
            self.readChar();
            return token.Token.gt;
        } else if (self.charIs(',')) {
            self.readChar();
            return token.Token.comma;
        } else if (self.charIs(';')) {
            self.readChar();
            return token.Token.semicolon;
        } else if (self.charIs(':')) {
            self.readChar();
            return token.Token.colon;
        } else if (self.charIs('(')) {
            self.readChar();
            return token.Token.lparen;
        } else if (self.charIs(')')) {
            self.readChar();
            return token.Token.rparen;
        } else if (self.charIs('{')) {
            self.readChar();
            return token.Token.lbrace;
        } else if (self.charIs('}')) {
            self.readChar();
            return token.Token.rbrace;
        } else if (self.charIs('[')) {
            self.readChar();
            return token.Token.lbracket;
        } else if (self.charIs(']')) {
            self.readChar();
            return token.Token.rbracket;
        } else if (self.charIs('"')) {
            const stringToken = token.Token{ .stringLiteral = self.readString() };
            self.readChar();
            return stringToken;
        } else {
            if (self.char) |char| {
                if (isLetter(char)) {
                    return lookupIdent(self.readIdentifier());
                } else if (isDigit(char)) {
                    return token.Token{ .int = self.readNumber() };
                } else {
                    self.readChar();
                    return token.Token{ .illegal = char };
                }
            } else {
                self.readChar();
                return token.Token.eof;
            }
        }
    }

    fn charIs(self: Self, expected: u8) bool {
        if (self.char) |char| {
            return char == expected;
        } else {
            return false;
        }
    }

    fn peekCharIs(self: Self, expected: u8) bool {
        if (self.peekChar()) |peekCh| {
            return peekCh == expected;
        } else {
            return false;
        }
    }

    fn isLetter(char: u8) bool {
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or char == '_' or char == '!' or char == '?';
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn readChar(self: *Self) void {
        self.char = self.peekChar();
        self.position = self.readPosition;
        self.readPosition += 1;
    }

    fn peekChar(self: Self) ?u8 {
        if (self.readPosition >= self.input.len) {
            return null;
        } else {
            return self.input[self.readPosition];
        }
    }

    fn readRange(self: Self, start: usize, end: usize) []const u8 {
        return self.input[start..end];
    }

    fn readIdentifier(self: *Self) []const u8 {
        const prevPosition = self.position;
        while (true) {
            if (self.char) |char| {
                if (isLetter(char)) {
                    self.readChar();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return self.readRange(prevPosition, self.position);
    }

    fn readNumber(self: *Self) []const u8 {
        const prevPosition = self.position;
        while (true) {
            if (self.char) |char| {
                if (isDigit(char)) {
                    self.readChar();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return self.readRange(prevPosition, self.position);
    }

    fn readString(self: *Self) []const u8 {
        const prevPosition = self.position + 1;
        while (true) {
            self.readChar();
            if (self.char) |char| {
                if (char == '"') {
                    break;
                }
            } else {
                break;
            }
        }
        return self.readRange(prevPosition, self.position);
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            if (self.char) |char| {
                switch (char) {
                    '\t', '\n', '\x0C', '\r', ' ' => {
                        self.readChar();
                    },
                    else => {
                        break;
                    },
                }
            } else {
                break;
            }
        }
    }
};

fn lookupIdent(ident: []const u8) token.Token {
    if (std.mem.eql(u8, ident, "fn")) {
        return token.Token.function;
    } else if (std.mem.eql(u8, ident, "let")) {
        return token.Token.let;
    } else if (std.mem.eql(u8, ident, "true")) {
        return token.Token.true_;
    } else if (std.mem.eql(u8, ident, "false")) {
        return token.Token.false_;
    } else if (std.mem.eql(u8, ident, "if")) {
        return token.Token.if_;
    } else if (std.mem.eql(u8, ident, "else")) {
        return token.Token.else_;
    } else if (std.mem.eql(u8, ident, "return")) {
        return token.Token.return_;
    } else if (std.mem.eql(u8, ident, "macro")) {
        return token.Token.macro;
    } else {
        return token.Token{ .ident = ident };
    }
}

test "lookupIdent" {
    try expect(lookupIdent("fn") == token.Token.function);
    try expect(lookupIdent("let") == token.Token.let);
    try expect(lookupIdent("true") == token.Token.true_);
    try expect(lookupIdent("false") == token.Token.false_);
    try expect(lookupIdent("if") == token.Token.if_);
    try expect(lookupIdent("else") == token.Token.else_);
    try expect(lookupIdent("return") == token.Token.return_);
    try expect(lookupIdent("macro") == token.Token.macro);

    const ident1 = lookupIdent("id");
    try expect(ident1 == .ident);
    switch (ident1) {
        token.Token.ident => |value| try expectEqualStrings("id", value),
        else => unreachable,
    }

    const ident2 = lookupIdent("id2");
    try expect(ident2 == .ident);
    switch (ident2) {
        token.Token.ident => |value| try expectEqualStrings("id2", value),
        else => unreachable,
    }
}

// test utils
fn expectStringInnerToken(expected: []const u8, actual: token.Token) !void {
    switch (actual) {
        token.Token.ident, token.Token.int, token.Token.stringLiteral => |value| try expectEqualStrings(expected, value),
        else => unreachable,
    }
}

fn expectIdent(expected: []const u8, actual: token.Token) !void {
    try expect(actual == .ident);
    try expectStringInnerToken(expected, actual);
}

fn expectInt(expected: []const u8, actual: token.Token) !void {
    try expect(actual == .int);
    try expectStringInnerToken(expected, actual);
}

fn expectStringLiteral(expected: []const u8, actual: token.Token) !void {
    try expect(actual == .stringLiteral);
    try expectStringInnerToken(expected, actual);
}

test "lexer" {
    const input =
        \\ let five = 5;
        \\ let ten = 10;
        \\ 
        \\ let add = fn(x, y) {
        \\   x + y;
        \\ };
        \\ 
        \\ let result = add(five, ten);
        \\ !-/*5;
        \\ 5 < 10 > 5;
        \\ 
        \\ if(5 < 10) {
        \\   return true;
        \\ } else {
        \\   return false;
        \\ }
        \\ 
        \\ 10 == 10;
        \\ 10 != 9;
        \\ "foobar"
        \\ "foo bar"
        \\ [1, 2];
        \\ {"foo": "bar"}
        \\ macro(x, y) { x + y; };
    ;

    var lexer = Lexer.new(input);
    try expectEqual(token.Token.let, lexer.nextToken());
    try expectIdent("five", lexer.nextToken());
    try expectEqual(token.Token.assign, lexer.nextToken());
    try expectInt("5", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.let, lexer.nextToken());
    try expectIdent("ten", lexer.nextToken());
    try expectEqual(token.Token.assign, lexer.nextToken());
    try expectInt("10", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.let, lexer.nextToken());
    try expectIdent("add", lexer.nextToken());
    try expectEqual(token.Token.assign, lexer.nextToken());
    try expectEqual(token.Token.function, lexer.nextToken());
    try expectEqual(token.Token.lparen, lexer.nextToken());
    try expectIdent("x", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectIdent("y", lexer.nextToken());
    try expectEqual(token.Token.rparen, lexer.nextToken());
    try expectEqual(token.Token.lbrace, lexer.nextToken());
    try expectIdent("x", lexer.nextToken());
    try expectEqual(token.Token.plus, lexer.nextToken());
    try expectIdent("y", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.rbrace, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.let, lexer.nextToken());
    try expectIdent("result", lexer.nextToken());
    try expectEqual(token.Token.assign, lexer.nextToken());
    try expectIdent("add", lexer.nextToken());
    try expectEqual(token.Token.lparen, lexer.nextToken());
    try expectIdent("five", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectIdent("ten", lexer.nextToken());
    try expectEqual(token.Token.rparen, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.bang, lexer.nextToken());
    try expectEqual(token.Token.minus, lexer.nextToken());
    try expectEqual(token.Token.slash, lexer.nextToken());
    try expectEqual(token.Token.asterisk, lexer.nextToken());
    try expectInt("5", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectInt("5", lexer.nextToken());
    try expectEqual(token.Token.lt, lexer.nextToken());
    try expectInt("10", lexer.nextToken());
    try expectEqual(token.Token.gt, lexer.nextToken());
    try expectInt("5", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.if_, lexer.nextToken());
    try expectEqual(token.Token.lparen, lexer.nextToken());
    try expectInt("5", lexer.nextToken());
    try expectEqual(token.Token.lt, lexer.nextToken());
    try expectInt("10", lexer.nextToken());
    try expectEqual(token.Token.rparen, lexer.nextToken());
    try expectEqual(token.Token.lbrace, lexer.nextToken());
    try expectEqual(token.Token.return_, lexer.nextToken());
    try expectEqual(token.Token.true_, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.rbrace, lexer.nextToken());
    try expectEqual(token.Token.else_, lexer.nextToken());
    try expectEqual(token.Token.lbrace, lexer.nextToken());
    try expectEqual(token.Token.return_, lexer.nextToken());
    try expectEqual(token.Token.false_, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.rbrace, lexer.nextToken());
    try expectInt("10", lexer.nextToken());
    try expectEqual(token.Token.equal, lexer.nextToken());
    try expectInt(
        "10",
        lexer.nextToken(),
    );
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectInt("10", lexer.nextToken());
    try expectEqual(token.Token.notEqual, lexer.nextToken());
    try expectInt("9", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectStringLiteral("foobar", lexer.nextToken());
    try expectStringLiteral("foo bar", lexer.nextToken());
    try expectEqual(token.Token.lbracket, lexer.nextToken());
    try expectInt("1", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectInt("2", lexer.nextToken());
    try expectEqual(token.Token.rbracket, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.lbrace, lexer.nextToken());
    try expectStringLiteral("foo", lexer.nextToken());
    try expectEqual(token.Token.colon, lexer.nextToken());
    try expectStringLiteral("bar", lexer.nextToken());
    try expectEqual(token.Token.rbrace, lexer.nextToken());
    try expectEqual(token.Token.macro, lexer.nextToken());
    try expectEqual(token.Token.lparen, lexer.nextToken());
    try expectIdent("x", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectIdent("y", lexer.nextToken());
    try expectEqual(token.Token.rparen, lexer.nextToken());
    try expectEqual(token.Token.lbrace, lexer.nextToken());
    try expectIdent("x", lexer.nextToken());
    try expectEqual(token.Token.plus, lexer.nextToken());
    try expectIdent("y", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.rbrace, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.eof, lexer.nextToken());
}
