/*
 * File name: data_structure.cpp
 * 
 * Authors: Abiola Raji, Ochihai Omuha
 * 
 * Implementation file for the core data structure functionality.
 * Contains the implementation of the get_interval function which
 * calculates time differences between Time objects.
 */

#include "data_structure.h"

int get_interval(Time start, Time end) {
    int start_minutes = start.hour * 60 + start.minute;
    int end_minutes = end.hour * 60 + end.minute;
    return end_minutes - start_minutes;
}
