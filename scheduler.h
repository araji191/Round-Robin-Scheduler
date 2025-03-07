#ifndef SCHEDULER_H
#define SCHEDULER_H

#include <iostream>
#include "data_structure.h"
using namespace std;

bool schedule_matches(Match matches[], int total_matchups, Tournament &tournament);
bool solve(Match matches[], int match_index, int total_matchups, Tournament &tournament);
bool is_valid(Match matches[], int match_index, Tournament &tournament);

#endif