// Finding the top-level form at a position (src/FormFinder.cpp), which
// extempore send, the REPL and the language server all rely on.

#include <gtest/gtest.h>

#include "FormFinder.h"

#include <string>
#include <string_view>

using namespace extemp::forms;

namespace {

const std::string_view kSong = R"xtm(;; a song
(define tempo 120)   ; "quoted (paren" in a comment

(define (play beat)
  (println "a string with ) and ( in it" #\( #\))
  (callback (*metro* (+ beat 1)) 'play (+ beat 1)))

#| a block comment (with parens |#
(bind-func osc:[double,double]*
  (lambda (freq) (* 0.1 (sin freq))))
)xtm";

std::string formAt(std::string_view Text, std::size_t Line, std::size_t Column) {
    const auto form = at(Text, offsetOf(Text, Line, Column));
    return form ? std::string(Text.substr(form->begin, form->end - form->begin)) : "<none>";
}

}  // namespace

TEST(FormFinder, FindsEachTopLevelForm) {
    const auto forms = topLevel(kSong);
    ASSERT_EQ(forms.size(), 3u);
    EXPECT_EQ(forms[0].line, 2u);
    EXPECT_EQ(forms[1].line, 4u);
    EXPECT_EQ(forms[2].line, 9u);
    for (const auto& form : forms) {
        EXPECT_TRUE(form.closed);
    }
}

TEST(FormFinder, TheEnclosingForm) {
    EXPECT_TRUE(formAt(kSong, 2, 3).starts_with("(define tempo"));
    const auto play = formAt(kSong, 5, 10);
    EXPECT_TRUE(play.starts_with("(define (play beat)"));
    EXPECT_TRUE(play.ends_with("'play (+ beat 1)))"));
}

TEST(FormFinder, DelimitersInStringsCommentsAndCharactersAreSkipped) {
    EXPECT_EQ(formAt(kSong, 2, 30), "(define tempo 120)");
    EXPECT_TRUE(formAt(kSong, 6, 1).starts_with("(define (play beat)"));
}

TEST(FormFinder, TypesWithBrackets) {
    EXPECT_TRUE(formAt(kSong, 10, 5).starts_with("(bind-func osc:[double,double]*"));
    EXPECT_TRUE(formAt(kSong, 10, 5).ends_with("(sin freq))))"));
}

TEST(FormFinder, BetweenFormsTakesTheOneBefore) {
    EXPECT_EQ(formAt(kSong, 3, 1), "(define tempo 120)");
    EXPECT_TRUE(formAt(kSong, 8, 3).starts_with("(define (play beat)"));
}

TEST(FormFinder, BeforeAnyFormTakesTheFirst) {
    EXPECT_EQ(formAt(kSong, 1, 1), "(define tempo 120)");
}

TEST(FormFinder, PositionsPastTheEndAreClamped) {
    EXPECT_TRUE(formAt(kSong, 400, 1).starts_with("(bind-func osc"));
    EXPECT_EQ(formAt(kSong, 2, 4000), "(define tempo 120)");
}

TEST(FormFinder, AnUnterminatedFormRunsToTheEnd) {
    const std::string_view text = "(a)\n(b (c)";
    const auto forms = topLevel(text);
    ASSERT_EQ(forms.size(), 2u);
    EXPECT_FALSE(forms[1].closed);
    EXPECT_EQ(forms[1].line, 2u);
    EXPECT_EQ(formAt(text, 2, 2), "(b (c)");
}

TEST(FormFinder, NoForms) {
    EXPECT_FALSE(at("; nothing\n", 0).has_value());
    EXPECT_TRUE(topLevel("").empty());
}

TEST(FormFinder, ColumnsCountCharactersNotBytes) {
    const std::string_view text = "(a \"\xC3\xA9\xC3\xA9\") (b)";  // (a "éé") (b)
    EXPECT_EQ(formAt(text, 1, 10), "(b)");  // the tenth character opens (b)
    EXPECT_EQ(formAt(text, 1, 8), "(a \"\xC3\xA9\xC3\xA9\")");
}

TEST(FormFinder, CompleteMeansNothingIsLeftOpen) {
    EXPECT_TRUE(complete("(+ 1 2)"));
    EXPECT_TRUE(complete("x"));
    EXPECT_TRUE(complete("(a) ; (not open"));
    EXPECT_FALSE(complete("(define x"));
    EXPECT_FALSE(complete("(println \"half a string"));
    EXPECT_FALSE(complete("#| half a comment"));
    EXPECT_TRUE(complete("(println \"(\")"));
}

TEST(FormFinder, ProblemsAReaderWouldTripOn) {
    EXPECT_TRUE(problems(kSong).empty());
    const auto stray = problems("(a))\n(b)");
    ASSERT_EQ(stray.size(), 1u);
    EXPECT_EQ(stray[0].offset, 3u);
    const auto open = problems("(a)\n(b (c)");
    ASSERT_EQ(open.size(), 1u);
    EXPECT_EQ(open[0].offset, 4u);
    EXPECT_EQ(problems("(println \"half").size(), 2u);  // the string, and the form around it
    EXPECT_EQ(problems("#| half").size(), 1u);
}

TEST(FormFinder, DefinitionsAreNamed) {
    const std::string_view text = R"xtm((bind-func osc:[double,double]*
  (lambda (freq) (* 0.1 (sin freq))))
(define tempo 120)
(define (play beat) beat)
(bind-type Pair <i64,i64>)
(bind-lib libc printf [i32,i8*]*)
(define-macro (twice x) `(begin ,x ,x))
(println 'not-a-definition)
(bind-val gain SAMPLE 0.5)
)xtm";
    const auto defs = definitions(text);
    ASSERT_EQ(defs.size(), 7u);
    const char* names[] = {"osc", "tempo", "play", "Pair", "printf", "twice", "gain"};
    const char* heads[] = {"bind-func", "define",       "define",  "bind-type",
                           "bind-lib",  "define-macro", "bind-val"};
    for (std::size_t i = 0; i < defs.size(); ++i) {
        EXPECT_EQ(defs[i].name, names[i]);
        EXPECT_EQ(defs[i].head, heads[i]);
        EXPECT_EQ(text.substr(defs[i].nameOffset, defs[i].name.size()), names[i]);
    }
    EXPECT_EQ(defs[2].form.line, 4u);
}

