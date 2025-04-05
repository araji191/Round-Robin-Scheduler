/*
 * File name: data_structure.h
 * 
 * Authors: Abiola Raji, Ochihai Omuha
 * 
 * This header file defines the core data structures and functions used for representing
 * the tournament scheduling system. It provides the fundamental building blocks for
 * time management, match representation, and tournament configuration.
 * 
 * Structures:
 * - Time: Represents a specific time of day with hour and minute components
 * - Match: Contains all details about a tournament match including participants, timing, and venue
 * - Tournament: Holds the complete configuration of a tournament including participants list and constraints
 * 
 * Functions:
 * - get_interval: Calculates the time difference between two Time objects in minutes
 */

#ifndef DATA_STRUCTURE_H
#define DATA_STRUCTURE_H

#include <string>
#include "constants.h"
using namespace std;

/**
 * @brief Represents a specific time of day in 24-hour format
 * 
 * Attributes:
 * - hour: Integer representing the hour (0-23)
 *          - 0 = midnight, 12 = noon, 23 = 11 PM
 * - minute: Integer representing minutes (0-59)
 *          - Must be a valid minute value
 */
struct Time {
    int hour;
    int minute;
};

/**
 * @brief Represents a single match in the tournament schedule
 * 
 * Attributes:
 * - start: Time object representing when the match begins
 * - end: Time object representing when the match concludes
 * - participant1: String containing name of first team/player
 * - participant2: String containing name of second team/player
 * - venue: Integer identifier for the match location (1-based index)
 * - day: Integer representing which tournament day the match occurs on (1-based index)
 * - scheduled: Boolean flag indicating if this match has been successfully placed in schedule
 */
struct Match {
    Time start;
    Time end;
    string participant1;
    string participant2;
    int venue;
    int day;
    bool scheduled;
};

/**
 * @brief Contains all configuration parameters for a tournament
 * 
 * Attributes:
 * - type: Integer indicating tournament type (1 = single round-robin, etc.)
 * - participants: Array of strings containing all participant names
 * - num_days: Integer count of days the tournament will run
 * - start_time: Time object for daily match start (earliest possible match time)
 * - end_time: Time object for daily match end (latest possible match time)
 * - match_length: Integer duration of matches in minutes
 * - num_venues: Integer count of available venues
 * - rest_period: Integer minimum minutes required between matches for participants
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

/**
 * @brief Calculates time difference between two Time objects
 * @param start The starting time
 * @param end The ending time
 * @return Time difference in minutes
 * 
 * Calculates the duration between two Time points by converting both to
 * total minutes since midnight and returning the difference.
 * Returns negative value if end time is before start time.
 */
int get_interval(Time start, Time end);

#endif