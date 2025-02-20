#include "data_structure.h"

#ifndef SCHEDULER_H
#define SCHEDULER_H

void schedule_matches(Match matches[MAX_MATCHUPS], int total_matchups, Tournament &tournament);
bool can_schedule(const Match &match, const Tournament &tournament, const Match schedule[MAX_VENUES_AND_DAYS][MAX_TIMESLOTS], int day, int venue, int timeslot);
bool dfs_schedule(Match matches[MAX_MATCHUPS], int total_matchups, Tournament &tournament, Match schedule[MAX_VENUES_AND_DAYS][MAX_TIMESLOTS], int match_index);

#endif