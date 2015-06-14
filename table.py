"""
This module provides functions for building a table.
Used for data directed programming style.
see section 3.3.3 in SICP
"""
from consLibrary import *

TABLE = Symbol("table")
LOOKUP_PROC = Symbol("lookup-proc")
INSERT_PROC = Symbol("insert-proc")


def assoc(key, records):
    """
    records: a list of pairs
    """
    if isNull(records):
        return False
    if isEq(key, caar(records)):
        return car(records)
    else:
        return assoc(key, cdr(records))

def lookup(key, table):
    """
    table: a headed list of records
    """
    record = assoc(key, cdr(table))
    if record:
        """
        return value
        """
        return cdr(record)
    else:
        return False

def insert(key, value, table):
    record = assoc(key, cdr(table))
    if record:
        set_cdr(record, value)
    else:
        set_cdr(table, cons(cons(key, value), cdr(table)))

def lookup2(key1, key2, table):
    """
    lookup key2 in a 2-dim table
    """
    subtable = assoc(key1, cdr(table))
    if subtable:
        record = assoc(key, cdr(subtable))
        if record:
            return cdr(record)
        else:
            return False
    else:
        return False

def insert2(key1, key2, value, table):
    """
    insert key2-value pair into a 2-dim table
    """
    subtable = assoc(key1, cdr(table))
    if subtable:
        insert(key2, value, subtable)
    else:
        set_cdr(table, cons(List(key1, cons(key2, val)), cdr(table)))
    print('insert2 done')

# creating a table as a procedure
def make_table():
    local_table = List(TABLE)

    def lookup(key1, key2):
        subtable = assoc(key1, cdr(local_table))
        if subtable:
            record = assoc(key2, cdr(subtable))
            if record:
                return cdr(record)
            else:
                return False
        return False

    def insert(key1, key2, value):
        subtable = assoc(key1, cdr(local_table))
        if subtable:
            # add or update record
            record = assoc(key2, cdr(subtable))
            if record:
                set_cdr(record, value)
            else:
                # add key2-value at front of subtable
                set_cdr(subtable, cons(cons(key2, value), cdr(subtable)))
        else:
            # add a subtable
            set_cdr(local_table, cons(List(key1, cons(key2, value)), cdr(local_table)))
        #pprint(local_table)

    def dispatch(m):
        if isEq(m, LOOKUP_PROC):
            return lookup
        elif isEq(m, INSERT_PROC):
            return insert
        else:
            raise ValueError("Unknown operation -- TABLE")
    return dispatch

operation_table = make_table()
get = operation_table(LOOKUP_PROC) # get(key1, key2)
put = operation_table(INSERT_PROC) # put(key1, key2, value)

