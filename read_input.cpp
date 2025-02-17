#include "read_input.h"

void read_input(ifstream &in, Tournament &tournament)
{
    string participant;
    in >> tournament.type;
    in.ignore();
    getline(in, participant);
    
    for (int i = 0; participant != "END"; i++)
    {
        tournament.participants[i] = participant;
        getline(in, participant);
    }

    in >> tournament.num_days;
    char colon;
    in >> tournament.start_time.hour >> colon >> tournament.start_time.minute;
    in >> tournament.end_time.hour >> colon >> tournament.end_time.minute;
    in >> tournament.match_length;
    in >> tournament.num_venues;
    in >> tournament.rest_period;
}