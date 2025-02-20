#include "scheduler.h"
#include <iostream>

using namespace std;

void schedule_matches(Match matches[MAX_MATCHUPS], int total_matchups, Tournament tournament)
{
    matches[0].venue = 1;
    matches[0].day = 1;
    matches[0].start.hour = 9;
    matches[0].start.minute = 0;
    matches[0].scheduled = true;

}