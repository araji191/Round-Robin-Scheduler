#ifndef DATA_STRUCTURE_H
#define DATA_STRUCTURE_H

#include <string>
#include "constants.h"
using namespace std;

struct Time {
    int hour, minute;
};

struct Match {
    Time start, end;
    string participant1, participant2;
    int venue, day;
    bool scheduled;
};

struct Tournament {
    int type;
    string participants[MAX_PARTICIPANTS];
    int num_days;
    Time start_time;
    Time end_time;
    int match_length;
    int num_venues;
    int rest_period;
};

int get_interval(Time start, Time end);

#endif
