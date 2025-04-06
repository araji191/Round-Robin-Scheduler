/*
 * File name: scheduler.cpp
 * 
 * Authors: Abiola Raji, Ochihai Omuha
 * 
 * Implementation file for the tournament scheduling algorithm.
 * Contains the core backtracking logic for assigning matches to time slots
 * while respecting all tournament constraints.
 * 
 * Functions:
 * - schedule_matches: Initializes and controls the scheduling process
 * - solve: Recursive backtracking implementation
 * - is_valid: Validates match assignments against constraints
 * - Helper functions for time calculations and conflict detection
 */

#include "scheduler.h"

int backtracks = 0;

bool schedule_matches(Match matches[], int total_matchups, Tournament &tournament) {
    // Validate that input parameters are divisible by 30
    if (tournament.match_length % 30 != 0 || 
        tournament.rest_period % 30 != 0 || 
        get_interval(tournament.start_time, tournament.end_time) % 30 != 0) {
        cout << "Error: Match length, rest period, and tournament time must be divisible by 30 minutes." << endl;
        return false;
    }

    // Clear the scheduler status for all matches
    for (int i = 0; i < total_matchups; i++) {
        matches[i].scheduled = false;
        matches[i].day = 0;
        matches[i].venue = 0;
        matches[i].start.hour = 0;
        matches[i].start.minute = 0;
        matches[i].end.hour = 0;
        matches[i].end.minute = 0;
    }
    
    // Reset backtrack counter
    backtracks = 0;
    
    //Backtracking
    bool success = solve(matches, 0, total_matchups, tournament);
    
    if (success) {
        cout << "Schedule created successfully!" << endl;
        return true;
    } else {
        cout << "A schedule was not able to be generated based on the input" << endl;
        return false;
    }
}

Time calculate_match_start_time(Tournament &tournament, int start_minute) {
    int total_start_minutes = (tournament.start_time.hour * 60 + tournament.start_time.minute) + start_minute;
    Time start;
    start.hour = total_start_minutes / 60;
    start.minute = total_start_minutes % 60;
    return start;
}

Time calculate_match_end_time(Time start, int match_length) {
    int total_end_minutes = start.hour * 60 + start.minute + match_length;
    Time end;
    end.hour = total_end_minutes / 60;
    end.minute = total_end_minutes % 60;
    return end;
}

bool solve(Match matches[], int match_index, int total_matchups, Tournament &tournament) {
    if (backtracks > MAX_BACKTRACKS) {
        return false;
    }

    if (match_index >= total_matchups) {
        return true;
    }
    
    int day_minutes = get_interval(tournament.start_time, tournament.end_time);
    
    for (int day = 1; day <= tournament.num_days; day++) {
        for (int venue = 1; venue <= tournament.num_venues; venue++) {
            for (int start_minute = 0; start_minute <= day_minutes - tournament.match_length; start_minute += 30) {
                Time start = calculate_match_start_time(tournament, start_minute);
                Time end = calculate_match_end_time(start, tournament.match_length);
                
                matches[match_index].day = day;
                matches[match_index].venue = venue;
                matches[match_index].start = start;
                matches[match_index].end = end;
                
                if (is_valid(matches, match_index, tournament)) {
                    matches[match_index].scheduled = true;
                    
                    if (solve(matches, match_index + 1, total_matchups, tournament)) {
                        return true;
                    }
                    
                    matches[match_index].scheduled = false;
                    backtracks++;
                }
            }
        }
    }
    
    return false;
}

bool has_venue_time_conflict(const Match &current, const Match &other) {
    int current_start = current.start.hour * 60 + current.start.minute;
    int current_end = current.end.hour * 60 + current.end.minute;
    int other_start = other.start.hour * 60 + other.start.minute;
    int other_end = other.end.hour * 60 + other.end.minute;
    
    return (current_start >= other_start && current_start < other_end) ||
           (other_start >= current_start && other_start < current_end);
}

bool has_participant_conflict(const Match &current, const Match &other) {
    return current.participant1 == other.participant1 ||
           current.participant1 == other.participant2 ||
           current.participant2 == other.participant1 ||
           current.participant2 == other.participant2;
}

bool is_valid(Match matches[], int match_index, Tournament &tournament) {
    Match &current = matches[match_index];
    
    for (int i = 0; i < match_index; i++) {
        if (!matches[i].scheduled) continue;
        
        // Venue and time conflict check
        if (current.day == matches[i].day && current.venue == matches[i].venue) {
            if (has_venue_time_conflict(current, matches[i])) {
                return false;
            }
        }
        
        // Participant conflict check
        if (has_participant_conflict(current, matches[i]) && current.day == matches[i].day) {
            int current_start = current.start.hour * 60 + current.start.minute;
            int current_end = current.end.hour * 60 + current.end.minute;
            int other_start = matches[i].start.hour * 60 + matches[i].start.minute;
            int other_end = matches[i].end.hour * 60 + matches[i].end.minute;
            
            if (has_venue_time_conflict(current, matches[i])) {
                return false;
            }
            
            // Check rest period
            if (abs(current_start - other_end) < tournament.rest_period ||
                abs(other_start - current_end) < tournament.rest_period) {
                return false;
            }
        }
    }
    
    return true;
}