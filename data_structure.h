/*
 * 
 * This header file defines the core data structures used for representing the tournament and match scheduling system.
 * 
 * The structures in this file model the essential components of the tournament system, such as time, matches, and tournaments.
 * This file also contains the declaration of the `get_interval` function, which is used to calculate the time difference between two `Time` objects.
 * 
 * Structures:
 * - Time: Represents a time of day using hours and minutes.
 * - Match: Represents a match in the tournament, including start and end times, participants, venue, and day of the match, along with the scheduling status.
 * - Tournament: Contains tournament details such as type, list of participants, number of days, start/end times, match length, number of venues, and rest period.
 * 
 * Functions:
 * - get_interval: Calculates the interval (in minutes) between two `Time` objects, which helps determine match durations and scheduling.
 * 
 */


#ifndef DATA_STRUCTURE_H
#define DATA_STRUCTURE_H

#include <string>
#include "constants.h"
using namespace std;


/*
 * Represents a specific time of day (hours and minutes).
 */

struct Time {
    int hour, minute;
};

/*
 * Represents a match in the tournament.
 * 
 * This structure holds details about the match, including the start and end times,
 * the participants (teams), the venue, the day it occurs, and whether the match
 * has been scheduled.
 */

struct Match {
    Time start, end;
    string participant1, participant2;
    int venue, day;
    bool scheduled;
};

/*
 * Represents the tournament details.
 * 
 * This structure holds information about the type of tournament, the participants,
 * the number of days, the start and end times, the match length, the number of venues,
 * and the rest period between matches.
 */

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

/*
 * Calculates the interval (in minutes) between two Time objects.
 * 
 * This function takes two `Time` objects and calculates the difference in minutes
 * between them, which is useful for calculating match durations and time differences.
 * 
 * start - The start time.
 * end - The end time.
 * return - The time difference in minutes.
 */


int get_interval(Time start, Time end);

#endif
