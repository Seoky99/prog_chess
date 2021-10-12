<ol><h1>TODO LIST</h1> 
<li> Finalize abstract type of board!!! Current implementation is almost? right but close. </li> 
  <li> Test abstract type!!! <strong> IN PARTICULAR, </strong> testing non square inputs of boards  </li> 
<li> Finish specs / signature of .mli !!! </li>
<li> from 1x1 to 9x9 data should have jsons in the data folder. Also make some non square chess boards. </li> 
  <li> Start coding up pieces so we can put them into the board impl </li> </ol>

<h2> Things to note </h2> <br /> 
When referencing functions of other modules, the functions must be specified in the .mli folder. However sometimes I noticed they still would be unbound, and
the only fix I found was running make clean, make build, dune build. 



# Upgraded Chess 

This project is a two player spinoff of chess that employs mechanics that allow progression for the player and customizations for pieces and the board.   </br> 


<h1> For reference : git commands </h1>

--This is for setup <br /> 
First obviously clone your local repository (git clone <link here>) <br />
Create your branch with git branch (name of branch)  <br />
git push origin (name of branch) (this should make your branch show on the website)  <br />
  
--This is for regular use <br />
git checkout (name of branch) <br />
git add . <br />
git commit -m "blah" <br />
git push origin (name of branch) <br />
(Then you go to github website and make a pull request) <br />
  
((To sync your branch with main:)) <br />
git checkout main <br />
git pull <br />
git checkout (name of branch) <br />
git merge main <br />
  
