import subprocess
import time

while True:
    # Execute git commands
    subprocess.run(['git', 'add', 'LaTeX_report/final_report.tex'])
    subprocess.run(['git', 'commit', '-m', 'Auto commit'])
    subprocess.run(['git', 'push'])

    # Wait for 5 minutes before running again
    time.sleep(300)
