#include <EXTClosureAddressTable.h>
#include <EXTLLVM.h>

#include <cstdio>
#include <cstdlib>
#include <string_view>

// The closure address table is an intrusive singly linked list whose layout
// generated code depends on (runtime/bitcode.ll declares %clsvar and
// add_address_table); do not turn it into a map.
namespace extemp {
namespace ClosureAddressTable {
EXPORT closure_address_table* get_address_table(const char* name, closure_address_table* table) {
    for (; table; table = table->next) {
        if (std::string_view(table->name) == name) {
            return table;
        }
    }
    return nullptr;
}

EXPORT uint32_t get_address_offset(uint64_t id, closure_address_table* table) {
    for (; table; table = table->next) {
        if (table->id == id) {
            return table->offset;
        }
    }
    return 0;
}

EXPORT bool check_address_exists(uint64_t id, closure_address_table* table) {
    for (; table; table = table->next) {
        if (table->id == id) {
            return true;
        }
    }
    return false;
}

EXPORT bool check_address_type(uint64_t id, closure_address_table* table, const char* type) {
    for (; table; table = table->next) {
        if (table->id != id) {
            continue;
        }
        if (std::string_view(table->type) != type &&
            std::string_view("{i8*, i8*, void (i8*, i8*)*}**") != type) {
            printf("Runtime Type Error: bad type %s for %s. Should be %s\n", type, table->name,
                   table->type);
            return false;
        }
        return true;
    }
    return false;
}

// alloctype 1 is the codegen's __make-closure-h path: a heap closure that
// lives for the rest of the run, so its table entries are malloc'd and never
// freed, deliberately. Everything else is allocated in the closure's zone and
// goes with it.
EXPORT closure_address_table* add_address_table(llvm_zone_t* zone, char* name, uint32_t offset,
                                                char* type, int alloctype,
                                                struct closure_address_table* table) {
    closure_address_table* t = nullptr;
    if (alloctype == 1) {
        t = static_cast<closure_address_table*>(malloc(sizeof(closure_address_table)));
    } else {
        t = static_cast<closure_address_table*>(
            extemp::EXTZones::llvm_zone_malloc(zone, sizeof(closure_address_table)));
    }
    t->id = string_hash(name);
    t->name = name;
    t->offset = offset;
    t->type = type;
    t->next = table;
    return t;
}
}  // namespace ClosureAddressTable
}  // namespace extemp
