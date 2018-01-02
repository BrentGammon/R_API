# Set up 

  - Install CRAN https://cran.r-project.org/mirrors.html
  - Open the R Console
  - In the R Console run this command to compile the r code 
```  
r <- plumb('/Users/brentgammon/Desktop/plumber.r')
```
- The location to where the file is stored on the file system
- To run
```  
r$run(port=8000)
```
- Then if running correctly you should be able to go to http://127.0.0.1:8000/__swagger__/
- which will contain documentation about the endpoints