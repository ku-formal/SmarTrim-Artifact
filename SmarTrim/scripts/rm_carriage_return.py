import sys
import os

DIR = sys.argv[1]

def main ():
  sol_list = [f for f in os.listdir(DIR) if f.endswith(".sol")]
  for filename in sol_list:
    with open (filename, 'w') as newfile:
      with open (os.path.join(DIR, filename), 'r') as oldfile:
        oldlines = oldfile.readlines()
        newlines = list(map (lambda l: l.replace('\r\n', '\n'), oldlines))
        newfile.writelines(newlines)
    os.system ('rm ' + os.path.join(DIR, filename))
    os.system ('mv ' + filename + ' ' + os.path.join(DIR, filename))
    
def check():
  sol_list = [f for f in os.listdir(DIR) if f.endswith(".sol")]
  for filename in sol_list:
    with open(os.path.join(DIR, filename), "rb") as fp:
      s = fp.read()
    if b"\r\n" in s:
      print(f"warning... {filename}")

if __name__ == "__main__":
  main()
