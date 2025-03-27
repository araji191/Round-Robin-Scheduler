/*
 * print_output.h
 * 
 * This header file declares the function used for printing the tournament schedule.
 * 
 * The function in this file is used to print the details of the scheduled matches, 
 * including the time, participants, venue, and day.
 * 
 * Function:
 * - print_schedule: Prints the details of all the scheduled matches to the console, including their start time, end time, participants, and assigned venue.
 * 
 */


#ifndef OUTPUT_H
#define OUTPUT_H

#include <iostream>
#include "data_structure.h"
using namespace std;


/*
 * Prints the tournament schedule to the console.
 * 
 * This function loops through the array of scheduled matches and prints their 
 * details, including the start time, end time, participants, venue, and day.
 * It is used to display the complete schedule after all matches have been successfully 
 * assigned to their respective time slots and venues.
 * 
 * matches - An array of `Match` objects containing all scheduled matches.
 * total_matchups - The total number of matches that need to be printed.
 */

void print_schedule(Match matches[], int total_matchups);

#endif
