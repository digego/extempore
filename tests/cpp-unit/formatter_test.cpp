// Formatting Extempore source (src/Formatter.cpp), which extempore fmt and the
// language server's formatting requests use.

#include <gtest/gtest.h>

#include "Formatter.h"

#include <string>
#include <string_view>

using extemp::format::format;

namespace {

// Text with every line's indentation removed, which formatting must restore.
std::string flatten(std::string_view Text) {
    std::string flat;
    bool lineStart = true;
    for (const char c : Text) {
        if (lineStart && c == ' ') {
            continue;
        }
        flat += c;
        lineStart = c == '\n';
    }
    return flat;
}

// Formatting the flattened text gives the text back, and formatting that
// changes nothing.
void expectFormatted(std::string_view Text) {
    const auto once = format(flatten(Text));
    ASSERT_TRUE(once.has_value()) << once.error();
    EXPECT_EQ(*once, Text);
    EXPECT_EQ(format(*once).value(), *once);
}

}  // namespace

TEST(Formatter, ACallAlignsWithItsFirstArgument) {
    expectFormatted("(play fmsynth (car cell)\n"
                    "      (cosr 90 10 2)\n"
                    "      dur)\n");
    expectFormatted("(if (< a 1)\n"
                    "    (println \"done\")\n"
                    "    (loop (- a 1)))\n");
}

TEST(Formatter, AHeadAloneOnItsLineAlignsWithTheHead) {
    expectFormatted("(cond\n"
                    " ((> x 0) 'positive)\n"
                    " (else 'other))\n");
}

TEST(Formatter, AListHeadedByAListAlignsWithItsFirstElement) {
    expectFormatted("(let ((a 1) (b 2)\n"
                    "      (c 3))\n"
                    "  (cond ((< a 2)\n"
                    "         (+ a b))\n"
                    "        (else c)))\n");
}

TEST(Formatter, SpecialFormsIndentTheirBody) {
    expectFormatted("(lambda (x)\n"
                    "  (* x x))\n");
    expectFormatted("(dotimes (i 10) ;; a comment isn't an argument\n"
                    "  (println i))\n");
    expectFormatted("(when (> x 0)\n"
                    "  (println x))\n");
    expectFormatted("(do ((i 0 (+ i 1)))\n"
                    "    ((= i 3))\n"
                    "  (println i))\n");
    expectFormatted("(let loop ((i 0)\n"
                    "           (acc '()))\n"
                    "  (loop (+ i 1) acc))\n");
}

TEST(Formatter, ADistinguishedArgumentOnItsOwnLineIndentsFour) {
    expectFormatted("(lambda\n"
                    "    (x)\n"
                    "  (* x x))\n");
}

TEST(Formatter, MoreArgumentsOnTheHeadsLineThanItDistinguishesAlign) {
    expectFormatted("(begin (println a)\n"
                    "       (loop (- a 1)))\n");
    expectFormatted("(lambda (x) (println x)\n"
                    "            x)\n");
}

TEST(Formatter, DefinitionsIndentTheirBody) {
    expectFormatted("(bind-func static ring_buffer\n"
                    "  (lambda (n:i64)\n"
                    "    (zalloc n)))\n");
    expectFormatted("(define (f x) (println x)\n"
                    "  x)\n");
    expectFormatted("(define-macro (twice e)\n"
                    "  `(begin ,e ,e))\n");
}

TEST(Formatter, OnlyDefineIsADefinitionPrefix) {
    expectFormatted("(default-value x\n"
                    "               y)\n");
    expectFormatted("(defined? 'x\n"
                    "          env)\n");    expectFormatted("(#_format #f \"~A\"\n"
                    "          x)\n");
}

TEST(Formatter, AFormWhichStartsMidLineIndentsFromItsOwnParen) {
    expectFormatted("(let ((f (lambda (x)\n"
                    "           (* x x))))\n"
                    "  f)\n");
    expectFormatted("(define take-token (lambda (p)\n"
                    "                     (vector-set! p 1 0)\n"
                    "                     p))\n");
    expectFormatted("(define (template)\n"
                    "  `(begin\n"
                    "     (set! counter 0)\n"
                    "     counter))\n");
}

TEST(Formatter, DataAlignsWithItsFirstElement) {
    expectFormatted("(define notes '(60 63 62\n"
                    "                67 72))\n");
    expectFormatted("(define steps (list (60 63 62\n"
                    "                     67 72)))\n");
    expectFormatted("(define v #(1 2 3\n"
                    "            4 5))\n");
    expectFormatted("(define cells '((a b)\n"
                    "                (c d)))\n");
}

TEST(Formatter, CommentsIndentLikeCode) {
    expectFormatted("(define (f x)\n"
                    "  ;; a comment\n"
                    "  ; and another\n"
                    "  x)\n");
    EXPECT_EQ(format("                ; at the top level\n(f)\n").value(),
              "; at the top level\n(f)\n");
}

TEST(Formatter, StringsAndBlockCommentsKeepTheirLines) {
    const std::string docstring = "(bind-func f\n"
                                  "  \"a docstring which runs on   \n"
                                  "     with its own indentation\"\n"
                                  "  (lambda () 1))\n";
    EXPECT_EQ(format(docstring).value(), docstring);
    const std::string block = "#| a block comment\n"
                              "      keeps its layout |#\n"
                              "(f)\n";
    EXPECT_EQ(format(block).value(), block);
}

TEST(Formatter, DelimitersInStringsCommentsAndCharactersDontCount) {
    expectFormatted("(list \"(\" #\\( ; (\n"
                    "      #\\) x)\n");
}

TEST(Formatter, WhitespaceAtTheEnds) {
    EXPECT_EQ(format("(f)   \n\n\n(g)\t\n\n\n").value(), "(f)\n\n\n(g)\n");
    EXPECT_EQ(format("(f)").value(), "(f)\n");
    EXPECT_EQ(format("").value(), "");
    EXPECT_EQ(format("(f\r\n\tx)\r\n").value(), "(f\r\n x)\r\n");
}

TEST(Formatter, OnlyWhitespaceChanges) {
    // each line's content (stripped) is unchanged and in place
    const std::string text = "(bind-func osc:[double,double]*\n"
                             "        (lambda (freq) (* 0.1 (sin freq))))\n";
    EXPECT_EQ(format(text).value(), "(bind-func osc:[double,double]*\n"
                                 "  (lambda (freq) (* 0.1 (sin freq))))\n");
}

TEST(Formatter, TextWhichDoesntReadIsNotFormatted) {
    const auto unclosed = format("(define x 1)\n(define (f y)\n  (+ y 1\n");
    ASSERT_FALSE(unclosed.has_value());
    EXPECT_EQ(unclosed.error(), "line 2, column 1: this form never ends: a closing parenthesis is missing");
    EXPECT_FALSE(format("(f))\n").has_value());
}
