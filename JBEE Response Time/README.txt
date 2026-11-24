REPLICATION FILES README

Article title: Pinocchio's Clock: Using Response Time to Detect Deception in Sender-Receiver Games
Journal: Journal of Behavioral and Experimental Economics
Authors: Xin Jiang and Bohan Ye


1. CONTENTS OF THE REPLICATION PACKAGE

long_data1.csv
    Anonymized, analysis-ready data for Experiment 1.
    The file contains 620 observations from 62 participants. Each participant
    completed 10 rounds, so each row represents one participant-round.

long_data2.csv
    Anonymized, analysis-ready data for Experiment 2.
    The file contains 420 observations from 42 participants. Each participant
    completed 10 rounds, so each row represents one participant-round.

analysis1.R
    Reads long_data1.csv and reproduces the reported analyses for Experiment 1,
    including Table 3, Figures 1-4, Tables 4-6, Appendix Table 7, Appendix
    Figure 7, and the associated statistical tests reported in the text.

analysis2.R
    Reads long_data2.csv and reproduces the reported analyses for Experiment 2,
    including Figures 5-6, Appendix Figure 8, and the associated statistical
    tests reported in the text.



2. DATA STRUCTURE AND VARIABLE DEFINITIONS

Variables shared by long_data1.csv and long_data2.csv

index
    Anonymous participant identifier. 

Gender
    Participant gender: 0 = male, 1 = female, and 2 = other.

round
    Experimental round, from 1 to 10.

observed_RT
    Participant's own response time as a sender, in seconds, rounded to the nearest integer.

Sender_RT
    Response time of the matched sender, rounded to the nearest integer.

message
    Detailed category of the participant's message. In Experiment 1 the values
    distinguish honest y, honest n, deceptive y, deceptive n, and messages sent
    without full information. In Experiment 2 the values distinguish true and
    false y and n messages.

Credibility
    Truth status of the participant's message: 1 = true/honest and 0 =
    false/dishonest.

Yes
    Total number of y messages sent by the participant across the 10 rounds.
    This participant-level variable is repeated on the participant's 10 rows.

OthersYes
    Total number of y messages sent across the 10 rounds by the matched sender.
    This matched-sender-level variable is repeated where applicable.

honesty
    Total number of true/honest messages sent by the participant across the 10
    rounds. This participant-level variable is repeated on the participant's
    10 rows.

type2
    Participant honesty classification. honest indicates that all 10 messages
    were true/honest; other or others indicates all remaining participants, defined as dishonest senders in the paper.

Report
    Participant's message: 1 = y and 0 = n.

OthersReport
    Message of the matched sender: 1 = y and 0 = n.

NRTDecision
    Receiver's decision when the sender's response time was not displayed (RU).
    1 = YES and 0 = NO.

RTDecision
    Receiver's decision when the sender's response time was displayed (RI).
    1 = YES and 0 = NO.

NRTtrust
    Indicator that the receiver's no-response-time decision followed or trusted
    the sender's message: 1 = yes and 0 = no.

RTtrust
    Indicator that the receiver's response-time decision followed or trusted
    the sender's message: 1 = yes and 0 = no.


Variables appearing only in long_data1.csv

Condition
    Experiment 1 treatment: 0 = SU and 1 = SA.

Display
    Number of dice outcomes displayed to the sender, from 0 to 6.

know
    Sender's information state. know_yes means the sender knew that the correct
    report was y; know_no means the sender knew that the correct report was n;
    unknown means the sender was not fully informed.

response
    Message classification based on the participant's information and report:
    honest, deceptive, or unknown.

unknown
    Total number of messages sent by the participant without full information
    across the 10 rounds. This participant-level variable is repeated on the
    participant's 10 rows.

