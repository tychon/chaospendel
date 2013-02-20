#!/usr/bin/env python

#Import Modules
import os, sys, pygame, math, time, re
from pygame.locals import *
if not pygame.font: print 'Warning, fonts disabled'

RED = (250, 0, 0)
BLUE = (0, 0, 250)
GREEN = (0, 250, 0)
BARCOLOR = GREEN

#classes for our game objects
class Pendulum(pygame.sprite.Sprite):
  def __init__(self, rect, l1, l2):
    pygame.sprite.Sprite.__init__(self) #call Sprite initializer
    self.rect = rect
    self.image = pygame.Surface((rect.width, rect.height))
    self.image.fill( (0, 0, 0) )
    l = rect.height/2.0 * (4./5.)
    self.l1 = l * (l1 / (l1+l2))
    self.l2 = l * (l2 / (l1+l2))
  
  def setAngles(self, angles):
    self.angles = angles
  
  def update(self):
    self.image.fill( (0, 0, 0) )
    middle = self.rect.height/2
    x1 = middle + self.l1 *  math.sin(self.angles[0])
    y1 = middle + self.l1 * -math.cos(self.angles[0])
    x2 = x1     + self.l2 *  math.sin(self.angles[1])
    y2 = y1     + self.l2 * -math.cos(self.angles[1])
    pygame.draw.line(self.image, RED, (middle, middle), (x1, y1) )
    pygame.draw.line(self.image, RED, (x1, y1), (x2, y2) )

class Bar(pygame.sprite.Sprite):
  def __init__(self, rect, minval, maxval, initval=0, fix=False, fixval=0):
    pygame.sprite.Sprite.__init__(self)
    self.rect = rect
    self.image = pygame.Surface((rect.width, rect.height))
    self.minval = minval
    self.maxval = maxval
    self.scale = self.rect.height / float(maxval-minval)
    self.val = initval
    self.fix = fix
    self.fixval = fixval
  
  def set_value(self, val):
    self.val = val
  
  def update(self):
    self.image.fill( (0, 0, 0) )
    if self.fix: fixpos = self.scale * (self.maxval - self.fixval)
    else:            fixpos = self.scale * self.maxval
    valpos = self.scale * (self.fixval - self.val)
    pygame.draw.rect(self.image, BARCOLOR, pygame.Rect(0, fixpos, self.rect.width, valpos))
    # blue zero line
    zeropos = self.scale * self.maxval
    pygame.draw.line(self.image, BLUE, (0, zeropos), (self.rect.width, zeropos))
    # red fix line
    pygame.draw.line(self.image, RED, (0, fixpos), (self.rect.width, fixpos))

class Fourier2Show(pygame.sprite.Sprite):
  def __init__(self, rect, maxval):
    pygame.sprite.Sprite.__init__(self)
    self.rect = rect
    self.image = pygame.Surface((rect.width, rect.height))
    self.maxval = maxval
    self.scale = (rect.height/2) / float(maxval)
    self.values2 = self.values1 = [ 0 for i in range(rect.width) ]
  
  def set_values(self, vals1, vals2):
    self.values1 = vals1
    self.values2 = vals2
  
  def update(self):
    self.image.fill( (0, 0, 0) )
    xpos = 0
    for val in self.values1:
      valy = self.rect.height/2-self.scale*val
      pygame.draw.line(self.image, GREEN, (xpos, valy), (xpos, self.rect.height/2))
      xpos += 1
    xpos = 0
    for val in self.values2:
      valy = self.rect.height/2+self.scale*val
      pygame.draw.line(self.image, GREEN, (xpos, self.rect.height/2), (xpos, valy))
      xpos += 1
    pygame.draw.line(self.image, BLUE, (0, self.rect.height/2), (self.rect.width, self.rect.height/2))
    

RECT_SIZE = 500
ENERGY_WIDTH = 100

ENERGY_DIFF_ROUNDING = 10000

def main():
  # read arguments
  args = sys.argv[1:]
  project_name = None
  req_fps = -1
  gototime = -1
  while len(args) > 0:
    arg = args.pop(0)
    if arg == "-fps":
      val = args.pop(0)
      if val != "default": req_fps = float(val)
    elif arg == "-time":
      gototime = float(args.pop(0))
    elif not project_name:
      project_name = arg
    else: print "Argument ignored: ", arg
  if not project_name:
    print "Give me some project name!"
    exit(1)
  
  
  ################
  # open info file
  print "reading "+project_name+".info ..."
  infof = open(project_name+".info")
  total_time = -1# This is the time duration of the simulation at opt_fps.
  opt_fps = -1   # This framerate would play the samples in the csv with real time.
  time_step = -1 # This should be 1/opt_fps
  l1 = -1 # Length of first pendulum
  l2 = -1 # Length of second pendulum
  fourier_rows = 0 # number of rows in pgm file
  fourier1_window = fourier2_window = -1 # size of fourier window
  fourier1_scale = fourier2_scale = 0
  for line in infof:
    res = re.match(r"\s*(\S+)\s*=\s*(\S+)", line)
    if res:
      if   res.group(1) == "time": total_time = float(res.group(2))
      elif res.group(1) == "opt_fps": opt_fps = float(res.group(2))
      elif res.group(1) == "time_step": time_step = float(res.group(2))
      elif res.group(1) == "l1": l1 = float(res.group(2))
      elif res.group(1) == "l2": l2 = float(res.group(2))
      elif res.group(1) == "t1min": t1min = float(res.group(2))
      elif res.group(1) == "v1min": v1min = float(res.group(2))
      elif res.group(1) == "t2min": t2min = float(res.group(2))
      elif res.group(1) == "v2min": v2min = float(res.group(2))
      elif res.group(1) == "tmin":  tmin  = float(res.group(2))
      elif res.group(1) == "vmin":  vmin  = float(res.group(2))
      elif res.group(1) == "emin":  emin  = float(res.group(2))
      elif res.group(1) == "t1max": t1max = float(res.group(2))
      elif res.group(1) == "v1max": v1max = float(res.group(2))
      elif res.group(1) == "t2max": t2max = float(res.group(2))
      elif res.group(1) == "v2max": v2max = float(res.group(2))
      elif res.group(1) == "tmax":  tmax  = float(res.group(2))
      elif res.group(1) == "vmax":  vmax  = float(res.group(2))
      elif res.group(1) == "emax":  emax  = float(res.group(2))
      elif res.group(1) == "fourier1_window": fourier1_window  = float(res.group(2))
      elif res.group(1) == "fourier1_pgm_scaling": fourier1_scale  = float(res.group(2))
      elif res.group(1) == "fourier2_window": fourier2_window  = float(res.group(2))
      elif res.group(1) == "fourier2_pgm_scaling": fourier2_scale  = float(res.group(2))
  infof.close()
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
  
  fourier_window = max(fourier1_window, fourier2_window)
  fourier_scale = min(fourier1_scale, fourier2_scale)
  if fourier_window > 0 and fourier_scale == 0.0: fourier_window = -1
  print "favoured framerate:", req_fps
  print "estimated time scale:", req_fps/opt_fps
  
  energymin = min(tmin, vmin, emin)
  energymax = max(tmax, vmax, emax)
  print "bar scale: min="+str(energymin)+" max="+str(energymax)
  
  ##############
  ## read first line of csv data
  ## This is needed for initializing the energy bars
  print "opening "+project_name+".csv ..."
  csvf = open(project_name+".csv")
  numberstrs = csvf.readline().split(",")
  if len(numberstrs[0]) is 0: return
  data = [float(x) for x in numberstrs ]
  start_energy = data[10]
  
  ##############################
  if fourier_window > 0:
    # Read first lines of pgm file
    # to initialize equalizer
    print "opening "+project_name+".pend1.pgm ..."
    pgmf = open(project_name+".pend1.pgm")
    pgmf.readline() # skip first line
    pgmrow = pgmf.readline().split()
    [fourier_freqn, fourier_rows] = [int(x) for x in pgmrow]
    [pgmmax] = pgmf.readline().split()
    pgmmax = int(pgmmax)
    pgmf2 = open(project_name+".pend2.pgm")
    pgmf2.readline() # skip first line
    pgmf2.readline() # skip second line
    [pgmmax2] = pgmf2.readline().split() # read out third line
    pgmmax2 = int(pgmmax2)
    pgmmax = max(pgmmax, pgmmax2)
    print "fourier window:", fourier_window
    print "fourier maximum value: "+str(pgmmax/fourier_scale)
    print "number of fourier samples: "+str(fourier_rows)
    print "number of frequ in 1 sample: "+str(fourier_freqn)
    print "opening "+project_name+".pend2.pgm ..."
  else: fourier_freqn = 0
  
  ##################
  #Initialize pygame
  print "initializing pygame ..."
  pygame.init()
  screen = pygame.display.set_mode((RECT_SIZE+ENERGY_WIDTH+fourier_freqn, RECT_SIZE))
  pygame.display.set_caption('Chaos')
  #pygame.mouse.set_visible(0)
  font = pygame.font.Font(None, 18)
  #Prepare Game Objects
  pend = Pendulum(pygame.Rect(0, 0, RECT_SIZE, RECT_SIZE), l1, l2)
  barnum = 7
  t1bar = Bar(pygame.Rect(RECT_SIZE, 0, ENERGY_WIDTH/barnum, RECT_SIZE), energymin, energymax, fix=True, fixval=data[4])
  v1bar = Bar(pygame.Rect(RECT_SIZE+ENERGY_WIDTH/barnum*3, 0, ENERGY_WIDTH/barnum, RECT_SIZE), energymin, energymax, fix=True, fixval=data[5])
  t2bar = Bar(pygame.Rect(RECT_SIZE+ENERGY_WIDTH/barnum*1, 0, ENERGY_WIDTH/barnum, RECT_SIZE), energymin, energymax, fix=True, fixval=data[6])
  v2bar = Bar(pygame.Rect(RECT_SIZE+ENERGY_WIDTH/barnum*4, 0, ENERGY_WIDTH/barnum, RECT_SIZE), energymin, energymax, fix=True, fixval=data[7])
  tbar = Bar(pygame.Rect(RECT_SIZE+ENERGY_WIDTH/barnum*2, 0, ENERGY_WIDTH/barnum, RECT_SIZE), energymin, energymax, fix=True, fixval=data[8])
  vbar = Bar(pygame.Rect(RECT_SIZE+ENERGY_WIDTH/barnum*5, 0, ENERGY_WIDTH/barnum, RECT_SIZE), energymin, energymax, fix=True, fixval=data[9])
  ebar = Bar(pygame.Rect(RECT_SIZE+ENERGY_WIDTH/barnum*6, 0, ENERGY_WIDTH/barnum, RECT_SIZE), energymin, energymax, fix=True, fixval=data[10])
  spritelist = [pend, t1bar, v1bar, t2bar, v2bar, tbar, vbar, ebar]
  if fourier_window > 0:
    fouriershow = Fourier2Show(pygame.Rect(RECT_SIZE+ENERGY_WIDTH, 0, fourier_freqn, RECT_SIZE), pgmmax)
    spritelist.append(fouriershow)
  allsprites = pygame.sprite.RenderPlain(spritelist)
  
  
  text_t1 = font.render("T1", True, (250, 0, 0))
  text_v1 = font.render("V1", True, (250, 0, 0))
  text_t2 = font.render("T2", True, (250, 0, 0))
  text_v2 = font.render("V2", True, (250, 0, 0))
  text_t = font.render("T", True, (250, 0, 0))
  text_v = font.render("V", True, (250, 0, 0))
  text_e = font.render("E", True, (250, 0, 0))
  
  samplenum = 0
  artificial_time = 0.0
  fouriersampnum = 0
  real_time_start = time.time()
  ######
  # skip some samples?
  if gototime > 0:
    framestoskip = int(opt_fps * gototime)
    print "skipping", framestoskip, "samples ..."
    for i in range(framestoskip):
      if csvf.readline() == '':
        csvf.close()
        pgmf.close()
        pgmf2.close()
        print "reached end of csv data."
        return
      samplenum += 1
      if fourier_window > 0 and samplenum > fourier_window/2 and fouriersampnum < fourier_rows:
        if pgmf.readline() == '' or pgmf2.readline() == '':
          csvf.close()
          pgmf.close()
          pgmf2.close()
          print "reached end of fourier data."
          return
        fouriersampnum += 1
    artificial_time = gototime
    real_time_start = time.time()-artificial_time
  
  ##########
  #Main Loop
  print "starting game loop ..."
  frameTimes = []
  
  runon = True
  while runon:
    loopstart = time.time()
    
    # Handle Input Events
    for event in pygame.event.get():
      if event.type == QUIT: # react to normal window closing action
        print "User input causing QUIT."
        runon = False
    
    # load line from csv file
    numberstrs = csvf.readline().split(",")
    if len(numberstrs[0]) is 0:
      runon = False
      break
    data = [float(x) for x in numberstrs ]
    pend.setAngles([data[0], data[1]])
    t1bar.set_value(data[4]);
    v1bar.set_value(data[5]);
    t2bar.set_value(data[6]);
    v2bar.set_value(data[7]);
    tbar.set_value(data[8]);
    vbar.set_value(data[9]);
    ebar.set_value(data[10]);
    samplenum += 1
    # load line from pgm file
    if fourier_window > 0:
      if samplenum > fourier_window/2 and fouriersampnum < fourier_rows:
        numberstrs = pgmf.readline().split()
        data1 = [float(x) for x in numberstrs]
        numberstrs = pgmf2.readline().split()
        data2 = [float(x) for x in numberstrs]
        fouriershow.set_values(data1, data2)
        fouriersampnum += 1
      else: fouriershow.set_values([0 for x in range(fourier_freqn)], [])
    
    #Draw Everything
    allsprites.update()
    allsprites.draw(screen)
    # draw text
    text1 = font.render("t/s="+str(artificial_time), True, (0, 250, 0))
    screen.blit(text1, (0, 0) )
    text3 = font.render("tscale="+str(round(artificial_time/(time.time()-real_time_start), 5)), True, (0, 250, 0))
    screen.blit(text3, (0, 15) )
    fps = round(len(frameTimes)/float(sum(frameTimes)), 2) if len(frameTimes) > 0 else float('nan')
    text2 = font.render("fps="+str(fps), True, (0, 250, 0))
    screen.blit(text2, (0, 30) )
    ediff = round((1-data[10]/start_energy) *ENERGY_DIFF_ROUNDING) / ENERGY_DIFF_ROUNDING * 100
    text4 = font.render("ediff="+str(ediff)+"%", True, (250, 0, 0))
    screen.blit(text4, (RECT_SIZE, RECT_SIZE-15) )
    screen.blit(text_t1, (RECT_SIZE, 0) )
    screen.blit(text_t2, (RECT_SIZE+ENERGY_WIDTH/barnum, 15) )
    screen.blit(text_t, (RECT_SIZE+ENERGY_WIDTH/barnum*2+2, 0) )
    screen.blit(text_v1, (RECT_SIZE+ENERGY_WIDTH/barnum*3, RECT_SIZE-15-15) )
    screen.blit(text_v2, (RECT_SIZE+ENERGY_WIDTH/barnum*4, RECT_SIZE-30-15) )
    screen.blit(text_v, (RECT_SIZE+ENERGY_WIDTH/barnum*5+2, RECT_SIZE-15-15) )
    screen.blit(text_e, (RECT_SIZE+ENERGY_WIDTH/barnum*6+2, RECT_SIZE-30-15) )
    pygame.display.flip()
    
    artificial_time += time_step
    sleeptime = 1.0/req_fps - (time.time() - loopstart)
    if sleeptime > 0: time.sleep(sleeptime)
    
    if len(frameTimes) > 30: frameTimes.pop()
    frameTimes.insert(0, time.time() - loopstart)
  
  print "end of data."
  print "closing files ..."
  csvf.close()
  if fourier_window > 0: pgmf.close()
  print "finish."

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()
