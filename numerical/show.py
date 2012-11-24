#/usr/bin/env python

#Import Modules
import os, sys, pygame, math, time, re
from pygame.locals import *
if not pygame.font: print 'Warning, fonts disabled'

#classes for our game objects
class Pendulum(pygame.sprite.Sprite):
  def __init__(self, screen, l1, l2):
    pygame.sprite.Sprite.__init__(self) #call Sprite initializer
    self.screen = screen
    self.image = pygame.Surface(screen.get_size())
    self.rect = self.image.get_rect()
    self.image.fill( (0, 0, 0) )
    l = self.rect.width/2.0 * (4./5.)
    self.l1 = l * (l1 / (l1+l2))
    self.l2 = l * (l2 / (l1+l2))
    print l, self.l1, self.l2
  
  def setAngles(self, angles):
    self.angles = angles
  
  def update(self):
    self.image.fill( (0, 0, 0) )
    middle = self.rect.width/2
    x1 = middle + self.l1 *  math.sin(self.angles[0])
    y1 = middle + self.l1 * -math.cos(self.angles[0])
    x2 = x1     + self.l2 *  math.sin(self.angles[1])
    y2 = y1     + self.l2 * -math.cos(self.angles[1])
    pygame.draw.line(self.image, (250, 0, 0), (middle, middle), (x1, y1) )
    pygame.draw.line(self.image, (250, 0, 0), (x1, y1), (x2, y2) )

def main():
  # read arguments
  args = sys.argv[1:]
  project_name = None
  req_fps = -1
  while len(args) > 0:
    arg = args.pop(0)
    if arg == "-fps":
      req_fps = float(args.pop(0))
    elif not project_name:
      project_name = arg
    else: print "Argument ignored: ", arg
  if not project_name:
    print "Give me some project name!"
    exit(1)
  
  # open info file
  print "reading "+project_name+".info ..."
  infof = open(project_name+".info")
  total_time = -1# This is the time duration of the simulation at opt_fps.
  opt_fps = -1   # This framerate would play the samples in the csv with real time.
  time_step = -1 # This should be 1/opt_fps
  l1 = -1 # Length of first pendulum
  l2 = -1 # Length of second pendulum
  for line in infof:
    res = re.match(r"\s*(\S+)\s*=\s*(\S+)", line)
    if res:
      if   res.group(1) == "time": total_time = float(res.group(2))
      elif res.group(1) == "opt_fps": opt_fps = float(res.group(2))
      elif res.group(1) == "time_step": time_step = float(res.group(2))
      elif res.group(1) == "l1": l1 = float(res.group(2))
      elif res.group(1) == "l2": l2 = float(res.group(2))
  print "estimated total time:", total_time
  print "optimal framerate:", opt_fps
  print "optimal time per frame:", time_step
  print "Lengths:", (l1, l2)
  if time <= 0 or opt_fps <= 0 or time_step <= 0 or l1 <= 0 or l2 <= 0:
    print "Invalid entries in "+project_name+".info"
    print "Values smaller or equal to 0 are not allowed."
    exit(1)
  if req_fps < 0:
    # since no argument for fps was given, we will try opt_fps
    req_fps = opt_fps
  
  print "favoured framerate:", req_fps
  print "estimated time zoom:", req_fps/opt_fps
  
  #Initialize pygame
  pygame.init()
  screen = pygame.display.set_mode((200, 200))
  pygame.display.set_caption('Chaos')
  pygame.mouse.set_visible(0)
  font = pygame.font.Font(None, 18)
  #Prepare Game Objects
  pend = Pendulum(screen, l1, l2)
  allsprites = pygame.sprite.RenderPlain((pend))
  
  # open data file
  print "opening "+project_name+".csv ..."
  f = open(project_name+".csv")
  
  artificial_time = 0.0
  real_time_start = time.time()
  frameTimes = []
  
  #Main Loop
  runon = True
  while runon:
    loopstart = time.time()
    
    # Handle Input Events
    for event in pygame.event.get():
      if event.type == QUIT: # react to normal window closing action
        print "User input causing QUIT."
        runon = False
    
    # load line from file
    numberstrs = f.readline().split(",")
    if len(numberstrs[0]) is 0:
      runon = False
      break
    ang = [float(x) for x in numberstrs ]
    pend.setAngles([ang[1], ang[3]])
    
    #Draw Everything
    allsprites.update()
    allsprites.draw(screen)
    # draw text
    text1 = font.render("t/s="+str(artificial_time), 1, (0, 250, 0))
    screen.blit(text1, (0, 0) )
    text3 = font.render("tscale="+str(round(artificial_time/(time.time()-real_time_start), 5)), 1, (0, 250, 0))
    screen.blit(text3, (0, 15) )
    fps = round(len(frameTimes)/float(sum(frameTimes)), 2) if len(frameTimes) > 0 else float('nan')
    text2 = font.render("fps="+str(fps), 1, (0, 250, 0))
    screen.blit(text2, (0, 30) )
    pygame.display.flip()
    
    artificial_time += time_step
    sleeptime = 1.0/req_fps - (time.time() - loopstart)
    if sleeptime > 0: time.sleep(sleeptime)
    
    if len(frameTimes) > 30: frameTimes.pop()
    frameTimes.insert(0, time.time() - loopstart)

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()
