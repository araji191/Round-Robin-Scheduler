#include "read_input.h"
#include "constants.h"
#include <iostream>

using namespace std;

bool validate_tournament_type(Tournament &tournament) {
    if (tournament.type > MAX_TOURNAMENT_TYPE) {
        cout << "Error: Exceeded maximum tournament type." << endl;
        return false;
    }
    return true;
}

bool read_participants(ifstream &in, Tournament &tournament) {
    string participant;
    int participant_count = 0;

    while (getline(in, participant)) {
        if (participant == "END") break;
        if (participant.empty()) {
            cout << "Error: Empty participant name found." << endl;
            return false;
        }
        if (participant_count >= MAX_PARTICIPANTS) {
            cout << "Error: Exceeded maximum participants (" << MAX_PARTICIPANTS << ")." << endl;
            return false;
        }
        // Check for duplicates
        for (int i = 0; i < participant_count; i++) {
            if (tournament.participants[i] == participant) {
                cout << "Error: Duplicate participant: \"" << participant << "\"" << endl;
                return false;
            }
        }
        tournament.participants[participant_count++] = participant;
    }

    return participant_count >= MIN_PARTICIPANTS;
}

bool validate_time(Time &time, const string &timeType) {
    if (time.hour < 0 || time.hour > 23 || time.minute < 0 || time.minute > 59) {
        cout << "Error: Invalid " << timeType << " time format." << endl;
        return false;
    }
    return true;
}

bool read_input(ifstream &in, Tournament &tournament) {
    // Read tournament type
    if (!(in >> tournament.type) || !validate_tournament_type(tournament)) 
        return false;

    // Read participants
    in.ignore();
    if (!read_participants(in, tournament)) {
        cout << "Error: At least " << MIN_PARTICIPANTS << " participants required." << endl;
        return false;
    }

    // Read tournament details
    if (!(in >> tournament.num_days) || tournament.num_days <= 0 || tournament.num_days > MAX_DAYS) {
        cout << "Error: Invalid number of days." << endl;
        return false;
    }

    // Read and validate times
    char colon;
    if (!(in >> tournament.start_time.hour >> colon >> tournament.start_time.minute) || 
        colon != ':' || !validate_time(tournament.start_time, "start")) 
        return false;

    if (!(in >> tournament.end_time.hour >> colon >> tournament.end_time.minute) || 
        colon != ':' || !validate_time(tournament.end_time, "end")) 
        return false;

    // Validate time range and match details
    if (get_interval(tournament.start_time, tournament.end_time) <= 0) {
        cout << "Error: End time must be after start time." << endl;
        return false;
    }

    if (!(in >> tournament.match_length) || tournament.match_length <= 0 || 
        tournament.match_length > MAX_MATCH_LENGTH_MINUTES ||
        tournament.match_length > get_interval(tournament.start_time, tournament.end_time)) {
        cout << "Error: Invalid match length." << endl;
        return false;
    }

    // Read venues and rest period
    if (!(in >> tournament.num_venues) || tournament.num_venues <= 0 || tournament.num_venues > MAX_VENUES) {
        cout << "Error: Invalid number of venues." << endl;
        return false;
    }

    if (!(in >> tournament.rest_period) || tournament.rest_period < 0 || 
        tournament.rest_period > MAX_REST_PERIOD_MINUTES) {
        cout << "Error: Invalid rest period." << endl;
        return false;
    }

    return true;
}