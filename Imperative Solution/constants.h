/*
 * File name: constants.h
 * 
 * Authors: Abiola Raji, Ochihai Omuha
 * 
 * This header file defines all the constant values used throughout the tournament scheduler project.
 * These constants establish limits and constraints for various tournament parameters to ensure
 * the program operates within reasonable bounds.
 * 
 * Constants:
 * - MAX_PARTICIPANTS: Maximum number of participants allowed in a tournament
 * - MIN_PARTICIPANTS: Minimum number of participants required for a tournament
 * - MAX_MATCHUPS: Maximum number of matches that can be scheduled
 * - MAX_VENUES: Maximum number of venues that can be used
 * - MAX_DAYS: Maximum duration of a tournament in days
 * - MAX_TOURNAMENT_TYPE: Maximum value for tournament type identifier
 * - MAX_MATCH_LENGTH_MINUTES: Maximum duration of a single match in minutes
 * - MAX_REST_PERIOD_MINUTES: Maximum rest period between matches in minutes
 * - MAX_BACKTRACKS: Maximum number of backtracking attempts allowed in scheduling
 */

#ifndef CONSTANTS_H
#define CONSTANTS_H

#define MAX_PARTICIPANTS 100
#define MIN_PARTICIPANTS 2
#define MAX_MATCHUPS 100
#define MAX_VENUES 10
#define MAX_DAYS 30
#define MAX_TOURNAMENT_TYPE 5
#define MAX_MATCH_LENGTH_MINUTES 300
#define MAX_REST_PERIOD_MINUTES 480
#define MAX_BACKTRACKS 10000

#endif