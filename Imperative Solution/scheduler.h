/*
 * File name: scheduler.h
 * 
 * Authors: Abiola Raji, Ochihai Omuha
 * 
 * This header file declares functions for the tournament scheduling algorithm.
 * The functions implement a backtracking solution to assign matches to time slots
 * while respecting all constraints.
 * 
 * Functions:
 * - schedule_matches: Main scheduling interface
 * - solve: Recursive backtracking implementation
 * - is_valid: Constraint validation checker
 * - Helper functions for time calculations and conflict detection
 */

#ifndef SCHEDULER_H
#define SCHEDULER_H

#include <iostream>
#include "data_structure.h"
using namespace std;

/**
 * @brief Main scheduling interface function
 * @param matches Array of matches to schedule
 * @param total_matchups Total number of matches
 * @param tournament Tournament configuration
 * @return True if schedule was successfully created
 */
bool schedule_matches(Match matches[], int total_matchups, Tournament &tournament);

/**
 * @brief Recursive backtracking scheduler
 * @param matches Array of matches to schedule
 * @param match_index Current match being scheduled
 * @param total_matchups Total number of matches
 * @param tournament Tournament configuration
 * @return True if remaining matches can be scheduled
 */
bool solve(Match matches[], int match_index, int total_matchups, Tournament &tournament);

/**
 * @brief Validates match scheduling constraints
 * @param matches Array of matches
 * @param match_index Match to validate
 * @param tournament Tournament configuration
 * @return True if match can be scheduled at current position
 */
bool is_valid(Match matches[], int match_index, Tournament &tournament);

/**
 * @brief Calculates match start time from minutes offset
 * @param tournament Tournament configuration
 * @param start_minute Minutes from daily start time
 * @return Calculated Time object
 */
Time calculate_match_start_time(Tournament &tournament, int start_minute);

/**
 * @brief Calculates match end time from start time
 * @param start Match start time
 * @param match_length Duration in minutes
 * @return Calculated end Time
 */
Time calculate_match_end_time(Time start, int match_length);

/**
 * @brief Checks for venue time conflicts between matches
 * @param current Match being scheduled
 * @param other Already scheduled match
 * @return True if conflicts exist
 */
bool has_venue_time_conflict(const Match &current, const Match &other);

/**
 * @brief Checks for participant conflicts between matches
 * @param current Match being scheduled
 * @param other Already scheduled match
 * @return True if same participants are involved
 */
bool has_participant_conflict(const Match &current, const Match &other);

#endif