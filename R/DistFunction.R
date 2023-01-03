#' Function to convert between different units of distance
#'
#' The function will convert from a one unit of distance to another,
#' with both the starting unit and ending unit being provided as parameters
#' in the function call
#'
#' @param x the number to be converted
#' @param startUnit the starting unit that needs to be converted
#' @param endUnit the unit that the data has to be converted to
#'
#' @return
#' @export
#'
#'@import ST403-final_Project
#'
#' @examples
#' convertDist(100,"cm","ft)
#'
#' convertDist(212,"meter","yards")
#'
#' convertDist(82,"miles","mm")
#'
convertDist <- function(x,startUnit,endUnit){

  if(startUnit == "meters"|startUnit =="meter"|startUnit=="m")
  {
    km <- x/1000
  }
  if(startUnit == "centimeters"|startUnit =="centimeter"|startUnit=="cm")
  {
    km <- x/100000
  }
  if(startUnit == "kilometers"|startUnit =="kilometer"|startUnit=="km")
  {
    km <- x
  }
  if(startUnit == "milliimeters"|startUnit =="millimeter"|startUnit=="mm")
  {
    km <- x/1000000
  }
  if(startUnit == "miles"|startUnit =="mile"|startUnit=="mi")
  {
    km <- x*1.60934
  }
  if(startUnit == "inches"|startUnit =="inch"|startUnit=="in")
  {
    km <- x/39370.1
  }
  if(startUnit == "feet"|startUnit =="foot"|startUnit=="ft")
  {
    km <- x/3280.84
  }
  if(startUnit == "yards"|startUnit =="yard"|startUnit=="yd")
  {
    km <- x*0.0009144
  }

  if(endUnit == "meters"|endUnit == "meter"|endUnit == "m" )
  {
    m <- km*1000
    return(m)
  }
  if(endUnit == "centimeters"|endUnit == "centimeter"|endUnit == "cm")
  {
    cm <- km*100000
    return(cm)
  }
  if(endUnit == "millimeters"|endUnit == "millimeter"|endUnit == "mm")
  {
    mm <- km*1000000
    return(mm)
  }
  if(endUnit == "miles"|endUnit == "mile"|endUnit == "mi")
  {
    mi <- km/1.609344
    return(mi)
  }
  if(endUnit == "inches"|endUnit == "inch"|endUnit == "in")
  {
    inch <- km*39370.1
    return(inch)
  }

  if(endUnit == "feet"|endUnit == "foot"|endUnit == "ft")
  {
    feet <- km*3280.84
    return(feet)
  }

  if(endUnit == "yards"|endUnit == "yard"|endUnit== "yd")
  {
    yard <- km/0.0009144
    return(yard)
  }
  else
  {
    warning("endUnit is not valid")
  }
}
