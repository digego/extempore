// Tests for the IR preamble selection (include/IRPreamble.h): the text that
// is prepended to each xtlang compile so it can reference earlier compiles
// and the runtime helpers in runtime/bitcode.ll.

#include <gtest/gtest.h>

#include "IRPreamble.h"

#include <string>
#include <vector>

using namespace extemp::IRPreamble;

namespace {

std::vector<std::string> identifiers(std::string_view text) {
    std::vector<std::string> out;
    forEachIdentifier(text, [&](std::string_view name) { out.emplace_back(name); });
    return out;
}

// A preamble table in the shape of the real one: a type that references
// another type, declarations and helpers that reference types, a global.
Entries sampleEntries() {
    Entries e;
    add(e.types, "mzone", "%mzone = type {i8*, i64, i64, i64, i8*, %mzone*}\n");
    add(e.types, "pair", "%pair = type {%mzone*, i64}\n");
    add(e.types, "unused", "%unused = type {i32}\n");
    add(e.funcDecls, "llvm_zone_malloc", "declare i8* @llvm_zone_malloc(%mzone*, i64) nounwind\n");
    add(e.funcDecls, "take_pair", "declare void @take_pair(%pair*)\n");
    add(e.funcDecls, "printf", "declare i32 @printf(i8*, ...)\n");
    add(e.funcDefs, "i64tod",
        "define private double @i64tod(i64 %a) alwaysinline\n{\n%return = sitofp i64 %a to "
        "double\nret double %return\n}\n");
    add(e.funcDefs, "llvm_now",
        "define private i64 @llvm_now() nounwind alwaysinline\n{\n%res = "
        "load i64, i64* @TIME\nret i64 %res\n}\n");
    add(e.globals, "TIME", "@TIME = external global i64\n");
    add(e.globals, "other_var", "@other_var = external global [1 x i8*]\n");
    return e;
}

}  // namespace

TEST(IRPreamble, ForEachIdentifierFindsGlobalsLocalsAndTypes) {
    auto names = identifiers("%r = call %mzone* @llvm_peek_zone_stack()\n"
                             "%x.1 = bitcast [4 x i8]* @gs2 to i8*\n"
                             "br label %then\n"
                             "%5 = add i64 %4, 1\n"  // numbered values are not names
                             "@-dash = external global i8\n");
    std::vector<std::string> expected = {"r",    "mzone", "llvm_peek_zone_stack", "x.1", "gs2",
                                         "then", "-dash"};
    EXPECT_EQ(names, expected);
}

TEST(IRPreamble, OwnNamesCoverDefinitionsDeclarationsTypesAndGlobals) {
    OwnNames own = ownNames("%closure = type {i8*, i8*}\r\n"
                            "declare i32 @puts(i8*)\n"
                            "define dllexport fastcc double @foo__3(i8* %z) {\nret double 0.0\n}\n"
                            "@foo_var = dllexport global [1 x i8*] [ i8* null ]\n"
                            "  %notatype = alloca i64\n");
    EXPECT_EQ(own.types.count("closure"), 1u);
    EXPECT_EQ(own.types.size(), 1u);
    EXPECT_EQ(own.funcs.count("puts"), 1u);
    EXPECT_EQ(own.funcs.count("foo__3"), 1u);
    EXPECT_EQ(own.funcs.size(), 2u);
    EXPECT_EQ(own.globals.count("foo_var"), 1u);
    EXPECT_EQ(own.globals.size(), 1u);
}

TEST(IRPreamble, CaptureTypeDefsAndExternalGlobals) {
    Entries e;
    captureTypeDefs(e, "  %wt = type double (i8*, i64)\r\n%mzone = type {i8*}\n%x = alloca i64\n");
    EXPECT_EQ(e.types.at("wt"), "%wt = type double (i8*, i64)\n");
    EXPECT_EQ(e.types.at("mzone"), "%mzone = type {i8*}\n");
    EXPECT_EQ(e.types.size(), 2u);

    captureExternalGlobals(e, "@TIME = external global i64 ; extemp::UNIV::TIME\n"
                              "@foo_var = dllexport global [1 x i8*] [ i8* null ]\n");
    EXPECT_EQ(e.globals.at("TIME"), "@TIME = external global i64 ; extemp::UNIV::TIME\n");
    EXPECT_EQ(e.globals.size(), 1u);

    // first definition wins
    captureTypeDefs(e, "%mzone = type {i64}\n");
    EXPECT_EQ(e.types.at("mzone"), "%mzone = type {i8*}\n");
}

TEST(IRPreamble, CaptureDeclaresAndDefinesInBitcodeStyle) {
    Entries e;
    captureDeclaresAndDefines(e, ";; comment\n"
                                 "declare i8* @llvm_zone_malloc(%mzone*, i64) nounwind\n"
                                 "define private i1 @impc_true() nounwind alwaysinline\n"
                                 "{\n"
                                 "  ret i1 1\n"
                                 "}\n"
                                 "\n"
                                 "define private i64 @i1toi64(i1 %a) alwaysinline {\n"
                                 "entry:\n"
                                 "%return = zext i1 %a to i64\n"
                                 "ret i64 %return\n"
                                 "}\n"
                                 "declare void @after(i32)\n");
    EXPECT_EQ(e.funcDecls.at("llvm_zone_malloc"),
              "declare i8* @llvm_zone_malloc(%mzone*, i64) nounwind\n");
    EXPECT_EQ(e.funcDecls.at("after"), "declare void @after(i32)\n");
    EXPECT_EQ(e.funcDecls.size(), 2u);
    EXPECT_EQ(e.funcDefs.at("impc_true"),
              "define private i1 @impc_true() nounwind alwaysinline\n{\n  ret i1 1\n}\n");
    EXPECT_EQ(e.funcDefs.at("i1toi64"),
              "define private i64 @i1toi64(i1 %a) alwaysinline {\nentry:\n%return = zext i1 %a "
              "to i64\nret i64 %return\n}\n");
    EXPECT_EQ(e.funcDefs.size(), 2u);
}

TEST(IRPreamble, SelectFollowsReferencesTransitivelyInCategoryOrder) {
    Entries e = sampleEntries();
    // take_pair pulls in %pair, which pulls in %mzone; llvm_now pulls in @TIME
    std::string preamble = select(e, "define void @f() {\n"
                                     "call void @take_pair(%pair* null)\n"
                                     "%t = call i64 @llvm_now()\n"
                                     "ret void\n}\n");
    EXPECT_EQ(preamble, "%mzone = type {i8*, i64, i64, i64, i8*, %mzone*}\n"
                        "%pair = type {%mzone*, i64}\n"
                        "declare void @take_pair(%pair*)\n"
                        "define private i64 @llvm_now() nounwind alwaysinline\n{\n%res = "
                        "load i64, i64* @TIME\nret i64 %res\n}\n"
                        "@TIME = external global i64\n");
}

TEST(IRPreamble, SelectLeavesOutWhatTheIRDefinesItself) {
    Entries e = sampleEntries();
    // An AOT file carrying its own declaration of printf, and a redefinition
    // of a function an earlier compile recorded a declaration for.
    add(e.funcDecls, "foo_maker", "declare i8* @foo_maker(i8*)\n");
    std::string preamble = select(e, "declare i32 @printf(i8*, ...)\n"
                                     "define i8* @foo_maker(i8* %z) {\n"
                                     "%s = call i32 @printf(i8* null)\n"
                                     "ret i8* %z\n}\n");
    EXPECT_EQ(preamble, "");
}

TEST(IRPreamble, SelectPrefersHelperDefinitionOverDeclarationAndIgnoresUnknownNames) {
    Entries e = sampleEntries();
    add(e.funcDecls, "i64tod", "declare double @i64tod(i64)\n");  // a stray declaration
    std::string preamble = select(e, "%v = call double @i64tod(i64 %n)\n"
                                     "call void @not_in_any_map()\n"
                                     "br label %pair\n");  // a label named like a type
    EXPECT_EQ(preamble, "%mzone = type {i8*, i64, i64, i64, i8*, %mzone*}\n"
                        "%pair = type {%mzone*, i64}\n"
                        "define private double @i64tod(i64 %a) alwaysinline\n{\n%return = sitofp "
                        "i64 %a to double\nret double %return\n}\n");
}

TEST(IRPreamble, SelectIsEmptyForIRWithNoReferences) {
    Entries e = sampleEntries();
    EXPECT_EQ(select(e, "define void @g() {\nret void\n}\n"), "");
    EXPECT_EQ(select(e, ""), "");
}
