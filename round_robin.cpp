// File name: round-robin.cpp
// Authors: Ochihai Omuha, Abiola Raji
// Purpose:

#include <iostream>
#include <fstream>
#include <cmath>
#include "constants.h"
#include "data_structure.h"
#include "scheduler.h"
#include "read_input.h"
#include "print_output.h"

using namespace std;

int main() {

    ifstream input_file;
    input_file.open("input1.txt");

    Tournament tournament;

    read_input(input_file, tournament);

    int num_participants = 0;
    while (!tournament.participants[num_participants].empty()) {
        num_participants++;
    }

    const int TOTAL_MATCHUPS = ((pow(num_participants, 2) - num_participants) / 2 );
    Match matches[TOTAL_MATCHUPS];

    int match_index = 0;
    for (int i = 0; i < num_participants - 1; i++) {
        for (int j = i + 1; j < num_participants; j++) {
            matches[match_index].participant1 = tournament.participants[i];
            matches[match_index].participant2 = tournament.participants[j];
            match_index++;
        }
    }

    int NUM_VENUES_AND_DAYS = tournament.num_venues * tournament.num_days;
    int NUM_TIMESLOTS = get_interval(tournament.start_time, tournament.end_time) / tournament.match_length;
    Match schedule[NUM_VENUES_AND_DAYS][NUM_TIMESLOTS];

    //schedule_matches(schedule, NUM_VENUES_AND_DAYS, NUM_TIMESLOTS);

    //print_schedule(schedule);

    

    return 0;
}