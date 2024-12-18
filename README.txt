
Time Summary - package for maintaining text->excel timesheet flow
-----------------------------------------------------------------

This package allows you to maintain a timesheet in a text file,
then transfer it to a template excel file of a certain format.
This is the format my workplace uses, generally. But it's not so 
hard to adapt.

Examples of both the presence text file and the template excel
are in example_data/

The presence file is a 3-column whitespace-separated table. 
Lines dedicated to days not worked will have the reason
in column two. Accepted values are seen in the example.
Days considered half-worked (half-vacation) will have 
"half" in the 4th column, after the exit time.

There are two executables produced:

* time-summary-exe  - just counts the number of worked hours and 
    compares to an arbitrary quota (42 hours/week). This may get 
    parameterized later.
    
* excel-update - performs the substitution. Generates a new file 
    with a name based on the incoming name and the employee name,
    in the working directory.

For both executables, run:

  stack run <exe> --help
  
to build, run, and see the command line required for actual work.
excel-update should run with the example files as so:

   stack run excel-update-exe 11


Dependencies
------------
* Stack, a Haskell distribution.


Notes
-----
Only tested on WSL.
