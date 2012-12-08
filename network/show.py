
import sys, socket, re
import pygame
from pygame.locals import *
if not pygame.font: print 'Warning, fonts disabled'

class Graph(pygame.sprite.Sprite):
  def __init__(self, rect, dots, minval, maxval):
    pygame.sprite.Sprite.__init__(self)
    self.rect = rect
    self.image = pygame.Surface((rect.width, rect.height))
    self.minval = minval
    self.maxval = maxval
    self.dots = dots
    self.scale = rect.height / float(maxval-minval)
    self.xstep = rect.width / float(dots)
    self.vals = []
  
  def push_val(self, val):
    self.vals.append(val)
    if len(self.vals) > self.dots: self.vals.pop(0)
  
  def update(self):
    self.image.fill( (0, 0, 0) )
    xpos = -2
    last_ypos = 0.0
    print len(self.vals), self.vals
    for val in self.vals:
      ypos = (self.maxval-val)*self.scale
      pygame.draw.line(self.image, (0, 255, 0), (xpos-self.xstep, last_ypos), (xpos, ypos))
      xpos += self.xstep
      last_ypos = ypos
      

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
  screen = pygame.display.set_mode((400, 400))
  pygame.display.set_caption('Chaos')
  #pygame.mouse.set_visible(0)
  font = pygame.font.Font(None, 18)
  #Prepare Game Objects
  graph = Graph(pygame.Rect(0, 0, 400, 400), 400, 660, 720)
  allsprites = pygame.sprite.RenderPlain([graph])
  
  ################
  # open socket
  s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
  s.connect(socketname)
  
  ##########
  #Main Loop
  print "starting game loop ..."
  prog = re.compile(r"\[(\d+)\](\d+)")
  data = ""
  runon = True
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
      #print repr(mo.group(2))
      data = data[mo.end():]
      graph.push_val(int(mo.group(2)))
    
    allsprites.update()
    allsprites.draw(screen)
    pygame.display.flip()
  
  s.close()

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()
