#include "scheduler.h"

bool schedule_matches(Match matches[], int total_matchups, Tournament &tournament) {
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

bool solve(Match matches[], int match_index, int total_matchups, Tournament &tournament) {
    //If all matches have been scheduled
    if (match_index >= total_matchups) {
        return true;
    }
    
    // Try all possible days
    for (int day = 1; day <= tournament.num_days; day++) {
        // Try all possible venues
        for (int venue = 1; venue <= tournament.num_venues; venue++) {
            // Calculate available time slots in a day
            int day_minutes = get_interval(tournament.start_time, tournament.end_time);
            int possible_slots = (day_minutes - tournament.match_length + 1);
            
            // Try each possible starting time
            for (int start_minute = 0; start_minute <= possible_slots; start_minute += 30) {
                Time start;
                int total_start_minutes = (tournament.start_time.hour * 60 + tournament.start_time.minute) + start_minute;
                start.hour = total_start_minutes / 60;
                start.minute = total_start_minutes % 60;
                
                Time end;
                int total_end_minutes = total_start_minutes + tournament.match_length;
                end.hour = total_end_minutes / 60;
                end.minute = total_end_minutes % 60;
                
                // Check if end time exceeds tournament end time
                if (end.hour > tournament.end_time.hour || 
                    (end.hour == tournament.end_time.hour && end.minute > tournament.end_time.minute)) {
                    continue;  // Skip this slot if it exceeds end time
                }
                
                // Set match details
                matches[match_index].day = day;
                matches[match_index].venue = venue;
                matches[match_index].start = start;
                matches[match_index].end = end;
                
                if (is_valid(matches, match_index, tournament)) {
                    matches[match_index].scheduled = true;
                    
                    if (solve(matches, match_index + 1, total_matchups, tournament)) {
                        return true;
                    }
                    
                    // Backtrack if scheduling fails
                    matches[match_index].scheduled = false;
                }
            }
        }
    }
    
    // No valid placement found
    return false;
}

bool is_valid(Match matches[], int match_index, Tournament &tournament) {
    Match &current = matches[match_index];
    
    // Check all previously scheduled matches
    for (int i = 0; i < match_index; i++) {
        if (!matches[i].scheduled) continue;
        
        // Check for venue and time conflicts
        if (current.day == matches[i].day && current.venue == matches[i].venue) {
            // Convert times to minutes for easier comparison
            int current_start = current.start.hour * 60 + current.start.minute;
            int current_end = current.end.hour * 60 + current.end.minute;
            int other_start = matches[i].start.hour * 60 + matches[i].start.minute;
            int other_end = matches[i].end.hour * 60 + matches[i].end.minute;
            
            // Check for overlap
            if ((current_start >= other_start && current_start < other_end) ||
                (other_start >= current_start && other_start < current_end)) {
                return false;  // Time conflict at the same venue
            }
        }
        
        // Check for participant conflicts
        bool shared_participant = 
            current.participant1 == matches[i].participant1 ||
            current.participant1 == matches[i].participant2 ||
            current.participant2 == matches[i].participant1 ||
            current.participant2 == matches[i].participant2;
            
        if (shared_participant && current.day == matches[i].day) {
            // Check for rest period violations on the same day
            int current_start = current.start.hour * 60 + current.start.minute;
            int current_end = current.end.hour * 60 + current.end.minute;
            int other_start = matches[i].start.hour * 60 + matches[i].start.minute;
            int other_end = matches[i].end.hour * 60 + matches[i].end.minute;
            
            if ((current_start >= other_start && current_start < other_end) ||
                (other_start >= current_start && other_start < current_end)) {
                return false;  // Simultaneous matches for the same participant
            }
            
            // Check rest period
            if ((other_end + tournament.rest_period > current_start && other_start < current_start) ||
                (current_end + tournament.rest_period > other_start && current_start < other_start)) {
                return false;  // Rest period violation
            }
        }
    }
    
    return true;  // No conflicts found
}