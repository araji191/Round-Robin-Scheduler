
/*
 * scheduler.h
 * 
 * This header file declares the functions for scheduling matches in the tournament.
 * 
 * The functions in this file are responsible for the core match scheduling logic of the tournament.
 * They implement a backtracking algorithm to recursively assign matches to available time slots and venues
 * while ensuring that all constraints (e.g., rest period, venue conflicts) are respected.
 * 
 * Functions:
 * - schedule_matches: Initiates the scheduling process by calling the backtracking function to schedule all matches.
 * - solve: A recursive backtracking function that attempts to schedule each match and backtracks when necessary.
 * - is_valid: Checks if the current match can be scheduled without violating constraints (rest period, venue conflicts).
 */

#ifndef SCHEDULER_H
#define SCHEDULER_H

#include <iostream>
#include "data_structure.h"
using namespace std;

/*
 * Initiates the scheduling process for the tournament.
 * 
 * This function starts the match scheduling by calling the `solve` function, which uses 
 * recursive backtracking to assign matches to time slots and venues while ensuring that all 
 * constraints are satisfied.
 * 
 * matches - An array of `Match` objects representing the matches to be scheduled.
 * total_matchups - The total number of matches to schedule.
 * tournament - A reference to the `Tournament` object containing tournament configuration and parameters.
 * 
 * return `true` if the matches are successfully scheduled, `false` otherwise.
 */

bool schedule_matches(Match matches[], int total_matchups, Tournament &tournament);

/*
 * Recursively schedules matches using backtracking.
 * 
 * This function attempts to schedule a match by trying different available time slots and venues. 
 * If scheduling the current match succeeds, it recursively attempts to schedule the next match.
 * If a valid schedule cannot be found, the function backtracks by undoing the last match's scheduling.
 * 
 * matches - An array of `Match` objects representing the matches to be scheduled.
 * match_index - The index of the match currently being scheduled.
 * total_matchups - The total number of matches to schedule.
 * tournament - A reference to the `Tournament` object containing tournament configuration and parameters.
 * 
 * return `true` if the current match and subsequent matches can be successfully scheduled, `false` otherwise.
 */

bool solve(Match matches[], int match_index, int total_matchups, Tournament &tournament);

/*
 * Checks if a match can be scheduled without violating constraints.
 * 
 * This function verifies that a match does not violate constraints such as the rest period between 
 * matches for each team and conflicts with other scheduled matches at the same venue.
 * 
 * matches - An array of `Match` objects representing the matches to be scheduled.
 * match_index - The index of the match currently being scheduled.
 * tournament - A reference to the `Tournament` object containing tournament configuration and parameters.
 * 
 * return `true` if the match can be scheduled, `false` if it violates any constraints.
 */

bool is_valid(Match matches[], int match_index, Tournament &tournament);

#endif
