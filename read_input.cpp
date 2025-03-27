#include "read_input.h"
#include "constants.h"
#include <iostream>

using namespace std;

bool read_input(ifstream &in, Tournament &tournament) {
    // Read tournament type
    if (!(in >> tournament.type)) {
        cout << "Error: Failed to read tournament type." << endl;
        return false;
    }
    if (tournament.type > MAX_TOURNAMENT_TYPE) {
        cout << "Error: Exceeded maximum number of type (" << MAX_PARTICIPANTS << ")." << endl;
        return false;
    }

   // Read participants
in.ignore(); // Ignore newline after tournament type
string participant;
int participant_count = 0;

while (getline(in, participant)) {
    if (participant == "END") break;

    if (participant.empty()) {
        cout << "Error: Empty participant name found." << endl;
        return false;
    }

    if (participant_count >= MAX_PARTICIPANTS) {
        cout << "Error: Exceeded maximum number of participants (" << MAX_PARTICIPANTS << ")." << endl;
        return false;
    }

    // Check for duplicate participants (case-sensitive)
    for (int i = 0; i < participant_count; i++) {
        if (tournament.participants[i] == participant) {
            cout << "Error: Duplicate participant found: \"" << participant << "\"" << endl;
            return false;
        }
    }

    tournament.participants[participant_count++] = participant;
}

    if (participant_count < MIN_PARTICIPANTS) {
        cout << "Error: At least " << MIN_PARTICIPANTS << " participants are required for a tournament." << endl;
        return false;
    }

    // Read number of days
    if (!(in >> tournament.num_days)) {
        cout << "Error: Failed to read number of days." << endl;
        return false;
    }
    if (tournament.num_days <= 0 || tournament.num_days > MAX_DAYS) {
        cout << "Error: Number of days must be between 1 and " << MAX_DAYS << "." << endl;
        return false;
    }

    // Read start and end times
    char colon;
    Time &start = tournament.start_time;
    Time &end = tournament.end_time;

    if (!(in >> start.hour >> colon >> start.minute)) {
        cout << "Error: Failed to read start time." << endl;
        return false;
    }
    if (colon != ':' || start.hour < 0 || start.hour > 23 || start.minute < 0 || start.minute > 59) {
        cout << "Error: Invalid start time format. Expected HH:MM (24-hour format)." << endl;
        return false;
    }

    if (!(in >> end.hour >> colon >> end.minute)) {
        cout << "Error: Failed to read end time." << endl;
        return false;
    }
    if (colon != ':' || end.hour < 0 || end.hour > 23 || end.minute < 0 || end.minute > 59) {
        cout << "Error: Invalid end time format. Expected HH:MM (24-hour format)." << endl;
        return false;
    }

    // Validate time range
    if (get_interval(start, end) <= 0) {
        cout << "Error: End time must be after start time." << endl;
        return false;
    }

    // Read match length
    if (!(in >> tournament.match_length)) {
        cout << "Error: Failed to read match length." << endl;
        return false;
    }
    if (tournament.match_length <= 0 || tournament.match_length > MAX_MATCH_LENGTH_MINUTES) {
        cout << "Error: Match length must be between 1 and " << MAX_MATCH_LENGTH_MINUTES << " minutes." << endl;
        return false;
    }

    // Validate match length fits within time window
    if (tournament.match_length > get_interval(start, end)) {
        cout << "Error: Match length exceeds available time window." << endl;
        return false;
    }

    // Read number of venues
    if (!(in >> tournament.num_venues)) {
        cout << "Error: Failed to read number of venues." << endl;
        return false;
    }
    if (tournament.num_venues <= 0 || tournament.num_venues > MAX_VENUES) {
        cout << "Error: Number of venues must be between 1 and " << MAX_VENUES << "." << endl;
        return false;
    }

    // Read rest period
    if (!(in >> tournament.rest_period)) {
        cout << "Error: Failed to read rest period." << endl;
        return false;
    }
    if (tournament.rest_period < 0 || tournament.rest_period > MAX_REST_PERIOD_MINUTES) {
        cout << "Error: Rest period must be between 0 and " << MAX_REST_PERIOD_MINUTES << " minutes." << endl;
        return false;
    }

    // Check if rest period is at least match length (optional validation)
    if (tournament.rest_period < tournament.match_length) {
        cout << "Warning: Rest period is shorter than match length. This may cause scheduling conflicts." << endl;
    }

    return true;
}