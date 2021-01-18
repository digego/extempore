#pragma once

#include <UNIV.h>

#include <cinttypes>

namespace extemp {
namespace ClosureAddressTable {
    ///////////////////////////////////////////////////////////////////////
    // This here for Extempore Compiler Runtime.
    // This is temporary and needs to replaced with something sensible!
    struct closure_address_table
    {
	uint64_t id;
	char *name;
	uint32_t offset;
	char *type;
	struct closure_address_table *next;
    };

    EXPORT closure_address_table* get_address_table(const char *name, extemp::ClosureAddressTable::closure_address_table *table);

    EXPORT uint32_t get_address_offset(uint64_t id, closure_address_table* table);
    EXPORT bool check_address_exists(uint64_t id, closure_address_table* table);
    EXPORT bool check_address_type(uint64_t id, closure_address_table* table, const char* type);
} // namespace ClosureAddressTable
} // namespace extemp
