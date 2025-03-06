#include "data_structure.h"

#ifndef SCHEDULER_H
#define SCHEDULER_H

void schedule_matches(Match matches[MAX_MATCHUPS], int TOTAL_MATCHUPS, Tournament tournament);

Time calculate_start_time(int day, int time_slot, Time start_time, int match_length);
Time calculate_end_time(Time start, int match_length);

bool is_valid(int match_index, Match matches[], Tournament tournament, int NUM_VENUES_AND_DAYS, int NUM_TIMESLOTS);

bool solve(Tournament tournament, Match matches[], int match_index, int NUM_VENUES_AND_DAYS, int NUM_TIMESLOTS);

#endif