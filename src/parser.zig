const std = @import("std");
const expectEqual = std.testing.expectEqual;

const token = @import("token.zig");
const lexer = @import("lexer.zig");

const Priority = enum { lowest, equals, lessgreater, sum, product, prefix, call, index };

const Parser = struct {
    lexer: lexer.Lexer,
    currentToken: token.Token,
    peekToken: token.Token,
    errors: std.ArrayList([]const u8),
};

test {
    try expectEqual(true, true);
}
