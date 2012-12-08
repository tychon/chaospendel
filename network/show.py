
import sys, socket, re
import pygame
from pygame.locals import *

from color import *
import graph

if not pygame.font: print 'Warning, fonts disabled'

RECT_SIZE = 400
AVERAGE_LENGTHs = [5, 10, 20]

def main():
  # read arguments
  args = sys.argv[1:]
  socketname = None
  while len(args) > 0:
    arg = args.pop(0)
    if not socketname:
      socketname = arg
    else: print "Argument ignored: ", arg
  if not socketname:
    print "Give me some socket name!"
    exit(1)
  
  
  ##################
  #Initialize pygame
  print "initializing pygame ..."
  pygame.init()
  screen = pygame.display.set_mode((RECT_SIZE, RECT_SIZE))
  pygame.display.set_caption('Chaos')
  font = pygame.font.Font(None, 18)
  #Prepare Game Objects
  graphic = graph.Graph(pygame.Rect(0, 0, RECT_SIZE, RECT_SIZE)
                      , RECT_SIZE*2, 660, 720
                      , [BLUE, (255, 100, 0), (255, 50, 0), RED])
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
  while runon:
    # Handle Input Events
    for event in pygame.event.get():
      if event.type == QUIT: # react to normal window closing action
        print "User input causing QUIT."
        runon = False
        break;
    
    newdata = s.recv(1024)
    if len(newdata) <= 0:
      print "end of data"
      break
    data = data+newdata
    while True:
      mo = prog.match(data)
      if not mo: break;
      data = data[mo.end():]
      val = int(mo.group(2))
      # new averages
      for i in range(len(AVERAGE_LENGTHs)):
        averageelems[i].append(val)
        if len(averageelems[i]) > AVERAGE_LENGTHs[i]:
          rollingaverages[i] -= averageelems[i].pop(0) / float(AVERAGE_LENGTHs[i])
        rollingaverages[i] += val / float(AVERAGE_LENGTHs[i])
      #print ":", rollingaverages
      # put new data into graph
      vals = [float(val)]
      vals.extend([int(x) for x in rollingaverages])
      graphic.push_val(vals)
    
    allsprites.update()
    allsprites.draw(screen)
    pygame.display.flip()
  
  s.close()

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()
