from __future__ import print_function
import subprocess
import sys

def normalize_output(output):
  out = output.replace('\n', '')
  out = out.replace('mean: ', '')
  return out

def run_command(command):
  return normalize_output(subprocess.check_output([command]))

def run(no_cond_sel=False):
  if not no_cond_sel:
    cs_fact0 = run_command("./cond_sel_fact0.bin")
    cs_fact1 = run_command("./cond_sel_fact1.bin")
    cs_fact2 = run_command("./cond_sel_fact2.bin")
    cs_c0    = run_command("./cond_sel_c0.bin")
    cs_c1    = run_command("./cond_sel_c1.bin")
    cs_c2    = run_command("./cond_sel_c2.bin")

    print('Running conditional select tests...')
    print('\tFaCT\t\tC')
    print('O0\t' + cs_fact0 + '\t' + cs_c0)
    print('O1\t' + cs_fact1 + '\t' + cs_c1)
    print('O2\t' + cs_fact2 + '\t' + cs_c2)

    print('\n')

  print('Running salsa tests...')
  print('Salsa\tFaCT\t\tC')

  print('O2\t', end=''); sys.stdout.flush()
  salsa_fact2 = run_command("./salsa_fact2.bin")
  print(salsa_fact2 + '\t', end=''); sys.stdout.flush()
  salsa_c2    = run_command("./salsa_c2.bin")
  print(salsa_c2)

if __name__ == '__main__':
  print("Running FaCT perf tests\n")
  run('--no-cond-sel' in sys.argv)
