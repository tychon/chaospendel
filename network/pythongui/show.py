
import sys, socket, re, time
import pygame
from pygame.locals import *

from color import *
import graph

if not pygame.font: print 'Warning, fonts disabled'

AVERAGE_LENGTHs = [50]

def main():
  # read arguments
  args = sys.argv[1:]
  socketname = None
  maxfps = 80
  windowwidth = 600
  windowheight = 400
  while len(args) > 0:
    arg = args.pop(0)
    if arg == "-fps":
      maxfps = int(args.pop(0))
    elif arg == "--width":
      windowwidth = int(args.pop(0))
    elif arg == "--height":
      windowheight = int(args.pop(0))
    elif not socketname:
      socketname = arg
    else: print "Argument ignored: ", arg
  if not socketname:
    print "Give me some socket name!"
    exit(1)
  spf = 1.0 / float(maxfps)
  
  ##################
  #Initialize pygame
  print "initializing pygame ..."
  pygame.init()
  screen = pygame.display.set_mode((windowwidth, windowheight))
  pygame.display.set_caption('Chaos')
  font = pygame.font.Font(None, 18)
  #Prepare Game Objects
  graphic = graph.Graph(pygame.Rect(0, 0, windowwidth, windowheight)
                      , windowwidth/2, 660, 720
                      , [(0, 0, 255), GREEN, (0, 100, 0), (100, 100, 255)])
  allsprites = pygame.sprite.RenderPlain([graphic])
  
  ################
  # open socket
  print "opening socket ..."
  s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
  s.connect(socketname)
  
  ##########
  #Main Loop
  print "starting game loop ..."
  prog = re.compile(r"\[(\d+)\](\d+)")
  data = ""
  runon = True
  rollingaverages = [0.0 for i in range(len(AVERAGE_LENGTHs))]
  averageelems = [[] for i in range(len(AVERAGE_LENGTHs))]
  lastaverage = 0
  lasttime = time.time()-spf
  while runon:
    # Handle Input Events
    for event in pygame.event.get():
      if event.type == QUIT: # react to normal window closing action
        print "User input causing QUIT."
        runon = False
        break
    
    # receive new data
    newdata = s.recv(1024)
    if len(newdata) <= 0:
      print "end of data"
      break
    data = data+newdata
    # handle new data (parse all the new values)
    while True:
      mo = prog.search(data)
      if not mo: break
      data = data[mo.end():]
      val = int(mo.group(2))
      # new averages
      for i in range(len(AVERAGE_LENGTHs)):
        averageelems[i].append(val)
        if len(averageelems[i]) > AVERAGE_LENGTHs[i]:
          rollingaverages[i] -= averageelems[i].pop(0) / float(AVERAGE_LENGTHs[i])
        rollingaverages[i] += val / float(AVERAGE_LENGTHs[i])
      # calc gradient
      gradient = rollingaverages[0] - lastaverage
      lastaverage = rollingaverages[0]
      sys.stderr.write(str(gradient)+'\n')
      # put new data into graph
      vals = [float(val), gradient*5+670, int(gradient*5+670)]
      #vals.extend([int(x) for x in rollingaverages])
      vals.extend(rollingaverages)
      graphic.push_val(vals)
      if abs(int(gradient*5)) >= 2: sys.stdout.write("!!!\n");
    
    currtime = time.time()
    if currtime - lasttime > spf:
      allsprites.update()
      allsprites.draw(screen)
      pygame.display.flip()
      lasttime = currtime
  
  s.close()

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()
