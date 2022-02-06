# colour
CONFIG = list() 
# CONFIG$cols = c('#00468B','#ED0000','#42B540','#0099B4','#925E9F') # BMJ
# CONFIG$cols = c('#00468BFF','#FDAF91FF','#42B540FF','#0099B4FF','#925E9FFF') # Lancet Dark
# CONFIG$cols = c('#9CD6E9', '#E0A6A1', '#C9E0B4', '#039FC6', '#9FADD4') # Lancet Pastel - blue, pink, green, turquiose, purple
CONFIG$cols = c('#E64B35FF', '#4DBBD5FF', '#00A087FF', '#3C5488FF', '#F39B7FFF', 
                '#8491B4FF', '#91D1C2FF', '#DC0000FF', '#7E6148FF', '#B09C85FF') # Nature


lightup = function(c, alpha)
{
  z=col2rgb(c)/255
  return(rgb(z[1],z[2],z[3],alpha))  # 0.125 for col3
}
CONFIG$colsLight1 = c();for(i in seq(CONFIG$cols))CONFIG$colsLight1[i] = lightup(CONFIG$cols[i], alpha = 0.6)
CONFIG$colsLight2 = c();for(i in seq(CONFIG$cols))CONFIG$colsLight2[i] = lightup(CONFIG$cols[i], alpha = 0.4)
CONFIG$colsLight3 = c();for(i in seq(CONFIG$cols))CONFIG$colsLight3[i] = lightup(CONFIG$cols[i], alpha = 0.25)

rm(lightup,i)
