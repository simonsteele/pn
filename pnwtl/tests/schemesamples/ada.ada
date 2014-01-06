WITH Spider;
PROCEDURE Draw_Box_with_1_Loop IS
------------------------------------------------------------------
--| draw 4 x 4 box with spider - use loop
--| Author: Michael B. Feldman, The George Washington University
--| Last Modified: July 1995
------------------------------------------------------------------

BEGIN -- Draw_Box_with_1_Loop

  Spider.Start;

  FOR Side IN 1..4 LOOP
    Spider.Step;
    Spider.Step;
    Spider.Step;
    Spider.Right;
  END LOOP;

  Spider.Quit;

END Draw_Box_with_1_Loop;