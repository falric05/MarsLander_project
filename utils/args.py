import argparse

DEFAULT_INPUT = 'ins-1'

def parse_args():
    parser = argparse.ArgumentParser(description='SWI Prolog .')

    parser.add_argument('--data', required=True, type=str, help='name of txt data file')
    parser.add_argument('--plot', required=False, action="store_true", help='show final solutions plot')
    
    args = parser.parse_args()
    return args