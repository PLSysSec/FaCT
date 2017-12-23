import subprocess

def normalize_output(output):
  out = output.replace('\n', '')
  out = out.replace('mean: ', '')
  return out

def run_command(command):
  return normalize_output(subprocess.check_output([command]))

def run():
  cs_fact0 = run_command("./cond_sel_fact0.bin")
  cs_fact1 = run_command("./cond_sel_fact1.bin")
  cs_fact2 = run_command("./cond_sel_fact2.bin")
  cs_c0    = run_command("./cond_sel_c0.bin")
  cs_c1    = run_command("./cond_sel_c1.bin")
  cs_c2    = run_command("./cond_sel_c2.bin")

  print 'Running conditional select tests...'
  print '\tFaCT\t\tC'
  print 'O0\t' + cs_fact0 + '\t' + cs_c0
  print 'O1\t' + cs_fact1 + '\t' + cs_c1
  print 'O2\t' + cs_fact2 + '\t' + cs_c2

  print '\n'

  print 'Running salsa tests...'
  salsa_fact0 = run_command("./salsa_fact0.bin")
  salsa_c0    = run_command("./salsa_c0.bin")
  print 'Salsa\tFaCT\t\tC'
  print 'O0\t' + salsa_fact0 + '\t' + salsa_c0
  salsa_fact1 = run_command("./salsa_fact1.bin")
  salsa_c1    = run_command("./salsa_c1.bin")
  print 'O1\t' + salsa_fact1 + '\t' + salsa_c1
  salsa_fact2 = run_command("./salsa_fact2.bin")
  salsa_c2    = run_command("./salsa_c2.bin")
  print 'O2\t' + salsa_fact2 + '\t' + salsa_c2

if __name__ == '__main__':
  print "Running FaCT perf tests\n"
  run()