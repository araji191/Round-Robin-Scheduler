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

    int total_matchups = (((pow(num_participants, 2) - num_participants) / 2) * tournament.type);
    Match matches[MAX_MATCHUPS];

    
    int match_index = 0;
    for (int k = 0; k < tournament.type; k++)
    {
        for (int i = 0; i < num_participants; i++)
        {
            for (int j = i + 1; j < num_participants; j++)
            {
                matches[match_index].participant1 = tournament.participants[i];
                matches[match_index].participant2 = tournament.participants[j];
                match_index++;
            }
        }
    }


matches[0].start.hour = 9; matches[0].start.minute = 0; matches[0].end.hour = 10; matches[0].end.minute = 30;
matches[0].participant1 = "TEAM A"; matches[0].participant2 = "TEAM B"; matches[0].day = 1; matches[0].venue = 1; matches[0].scheduled = true;

matches[1].start.hour = 12; matches[1].start.minute = 0; matches[1].end.hour = 13; matches[1].end.minute = 30;
matches[1].participant1 = "TEAM A"; matches[1].participant2 = "TEAM C"; matches[1].day = 1; matches[1].venue = 1; matches[1].scheduled = true;

matches[2].start.hour = 15; matches[2].start.minute = 0; matches[2].end.hour = 16; matches[2].end.minute = 30;
matches[2].participant1 = "TEAM A"; matches[2].participant2 = "TEAM D"; matches[2].day = 1; matches[2].venue = 1; matches[2].scheduled = true;

matches[3].start.hour = 9; matches[3].start.minute = 0; matches[3].end.hour = 10; matches[3].end.minute = 30;
matches[3].participant1 = "TEAM C"; matches[3].participant2 = "TEAM D"; matches[3].day = 1; matches[3].venue = 2; matches[3].scheduled = true;

matches[4].start.hour = 12; matches[4].start.minute = 0; matches[4].end.hour = 13; matches[4].end.minute = 30;
matches[4].participant1 = "TEAM B"; matches[4].participant2 = "TEAM D"; matches[4].day = 1; matches[4].venue = 2; matches[4].scheduled = true;

matches[5].start.hour = 15; matches[5].start.minute = 0; matches[5].end.hour = 16; matches[5].end.minute = 30;
matches[5].participant1 = "TEAM B"; matches[5].participant2 = "TEAM E"; matches[5].day = 1; matches[5].venue = 2; matches[5].scheduled = true;

matches[6].start.hour = 9; matches[6].start.minute = 0; matches[6].end.hour = 10; matches[6].end.minute = 30;
matches[6].participant1 = "TEAM A"; matches[6].participant2 = "TEAM E"; matches[6].day = 2; matches[6].venue = 1; matches[6].scheduled = true;

matches[7].start.hour = 10; matches[7].start.minute = 30; matches[7].end.hour = 12; matches[7].end.minute = 0;
matches[7].participant1 = "TEAM B"; matches[7].participant2 = "TEAM C"; matches[7].day = 2; matches[7].venue = 1; matches[7].scheduled = true;

matches[8].start.hour = 12; matches[8].start.minute = 0; matches[8].end.hour = 13; matches[8].end.minute = 30;
matches[8].participant1 = "TEAM D"; matches[8].participant2 = "TEAM E"; matches[8].day = 2; matches[8].venue = 1; matches[8].scheduled = true;

matches[9].start.hour = 15; matches[9].start.minute = 0; matches[9].end.hour = 16; matches[9].end.minute = 30;
matches[9].participant1 = "TEAM C"; matches[9].participant2 = "TEAM E"; matches[9].day = 2; matches[9].venue = 1; matches[9].scheduled = true;

matches[10].start.hour = 9; matches[10].start.minute = 0; matches[10].end.hour = 10; matches[10].end.minute = 30;
matches[10].participant1 = "TEAM B"; matches[10].participant2 = "TEAM A"; matches[10].day = 3; matches[10].venue = 1; matches[10].scheduled = true;

matches[11].start.hour = 12; matches[11].start.minute = 0; matches[11].end.hour = 13; matches[11].end.minute = 30;
matches[11].participant1 = "TEAM C"; matches[11].participant2 = "TEAM A"; matches[11].day = 3; matches[11].venue = 1; matches[11].scheduled = true;

matches[12].start.hour = 15; matches[12].start.minute = 0; matches[12].end.hour = 16; matches[12].end.minute = 30;
matches[12].participant1 = "TEAM D"; matches[12].participant2 = "TEAM A"; matches[12].day = 3; matches[12].venue = 1; matches[12].scheduled = true;

matches[13].start.hour = 9; matches[13].start.minute = 0; matches[13].end.hour = 10; matches[13].end.minute = 30;
matches[13].participant1 = "TEAM D"; matches[13].participant2 = "TEAM C"; matches[13].day = 3; matches[13].venue = 2; matches[13].scheduled = true;

matches[14].start.hour = 12; matches[14].start.minute = 0; matches[14].end.hour = 13; matches[14].end.minute = 30;
matches[14].participant1 = "TEAM D"; matches[14].participant2 = "TEAM B"; matches[14].day = 3; matches[14].venue = 2; matches[14].scheduled = true;

matches[15].start.hour = 15; matches[15].start.minute = 0; matches[15].end.hour = 16; matches[15].end.minute = 30;
matches[15].participant1 = "TEAM E"; matches[15].participant2 = "TEAM B"; matches[15].day = 3; matches[15].venue = 2; matches[15].scheduled = true;

matches[16].start.hour = 9; matches[16].start.minute = 0; matches[16].end.hour = 10; matches[16].end.minute = 30;
matches[16].participant1 = "TEAM E"; matches[16].participant2 = "TEAM A"; matches[16].day = 4; matches[16].venue = 1; matches[16].scheduled = true;

matches[17].start.hour = 10; matches[17].start.minute = 30; matches[17].end.hour = 12; matches[17].end.minute = 0;
matches[17].participant1 = "TEAM C"; matches[17].participant2 = "TEAM B"; matches[17].day = 4; matches[17].venue = 1; matches[17].scheduled = true;

matches[18].start.hour = 12; matches[18].start.minute = 0; matches[18].end.hour = 13; matches[18].end.minute = 30;
matches[18].participant1 = "TEAM E"; matches[18].participant2 = "TEAM D"; matches[18].day = 4; matches[18].venue = 1; matches[18].scheduled = true;

matches[19].start.hour = 15; matches[19].start.minute = 0; matches[19].end.hour = 16; matches[19].end.minute = 30;
matches[19].participant1 = "TEAM E"; matches[19].participant2 = "TEAM C"; matches[19].day = 4; matches[19].venue = 1; matches[19].scheduled = true;

print_schedule(matches, total_matchups);

return 0;
    
}

