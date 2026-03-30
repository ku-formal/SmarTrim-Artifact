import os
import sys
import argparse

img = "verismart"

def run (pgm, outdir):
  cmd = ["docker", "run", "--rm",
         "--volume", os.path.dirname (pgm) + ":/tmp:ro",
         "--volume", outdir + ":/home/opam/VeriSmart/output",
         img, "-input", os.path.join ("/tmp", os.path.basename(pgm)),
         "-z3timeout", "20000", "-verify_timeout", "180",
         "-solc", "0.4.25", "-json"
        ]
  os.system(" ".join (cmd))

def main ():
  parser = argparse.ArgumentParser()
  parser.add_argument ('--pgm', type=str)
  parser.add_argument ('--outdir', type=str)
  parser.add_argument ('--version', default=False, action='store_true')

  args = parser.parse_args ()

  if args.version:
    print ("0.1.0")
  else:
    run (os.path.abspath(args.pgm), os.path.abspath(args.outdir))

if __name__ == "__main__":
  main ()
