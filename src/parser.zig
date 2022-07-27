const std = @import("std");
const expectEqual = std.testing.expectEqual;

const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

const Priority = enum { lowest, equals, lessgreater, sum, product, prefix, call, index };

const Parser = struct {
    lexer: Lexer,
    currentToken: Token,
    peekToken: Token,
    errors: std.ArrayList([]const u8),

    fn new(lexer: Lexer) Parser {
        const currentToken = lexer.nextToken();
        const peekToken = lexer.nextToken();

        return Parser{
            .lexer = lexer,
            .currentToken = currentToken,
            .peekToken = peekToken,
        };
    }
};

test {
    try expectEqual(true, true);
}
