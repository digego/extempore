#include <EXTClosureAddressTable.h>
#include <EXTLLVM.h>

#include <cstring>

namespace extemp {
namespace ClosureAddressTable {
EXPORT closure_address_table * get_address_table(const char *name, closure_address_table *table)
{
    while (table) {
        if (strcmp(table->name, name))
            return table;
        table = table->next;
    }
    // printf("Unable to locate %s in closure environment a\n", name);
    return 0;
}

EXPORT uint32_t get_address_offset(uint64_t id, closure_address_table* table)
{
    while(table)
    {
        // printf("%p name: %s\ntablename: %s\n\n", name, name, table->name);
        if(table->id == id) {
            // printf("in %s returning offset %d from %s\n", table->name, table->offset, name);
            return table->offset;
        }
        table = table->next;
    }
    // printf("Unable to locate %" PRIu64 " in closure environment b\n", id);
    return 0;
}

EXPORT bool check_address_exists(uint64_t id, closure_address_table* table)
{
    do {
        if (table->id == id) {
            return true;
        }
        table = table->next;
    } while (table);
    return false;
}

EXPORT bool check_address_type(uint64_t id, closure_address_table* table, const char* type)
{
    while(table)
    {
        if(table->id == id) {
            if((strcmp(table->type, type)!=0) && (strcmp("{i8*, i8*, void (i8*, i8*)*}**", type) != 0)) {
                printf("Runtime Type Error: bad type %s for %s. Should be %s\n", type, table->name, table->type);
                return 0;
            }
            else {
                return 1;
            }
        }
        table = table->next;
    }
    // printf("Unable to locate id in closure environment type: %s d\n",type);
    return 0;
}

static uint64_t string_hash(const char* str)
{
    uint64_t result(0);
    unsigned char c;
    while((c = *(str++))) {
        result = result * 33 + uint8_t(c);
    }
    return result;
}

EXPORT closure_address_table* add_address_table(llvm_zone_t* zone, char* name, uint32_t offset, char* type, int alloctype, struct closure_address_table* table)
{
    struct closure_address_table* t = NULL;
    if (alloctype == 1) {
        t = reinterpret_cast<closure_address_table*>(malloc(sizeof(struct closure_address_table)));
    } else {
        t = (struct closure_address_table*) extemp::EXTLLVM::llvm_zone_malloc(zone,sizeof(struct closure_address_table));
    }
    t->id = string_hash(name);
    t->name = name;
    t->offset = offset;
    t->type = type;
    t->next = table;
    return t;
}
} // namespace ClosureAddressTable
} // namespace extemp
