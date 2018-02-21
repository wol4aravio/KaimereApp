import os
import imageio
import matplotlib.pyplot as plt
from argparse import ArgumentParser, RawTextHelpFormatter


def gather_files(target_dirs):
    file_names = []
    for target_dir in target_dirs:
        entrails = os.listdir(target_dir)
        dirs = list(filter(lambda x: os.path.isdir(target_dir + '/' + x), entrails))
        files = list(filter(lambda x: not os.path.isdir(target_dir + '/' + x) and x.endswith('tif'), entrails))
        if len(dirs) == 0 and len(files) != 0:
            file_names += list(map(lambda x: target_dir + '/' + x, sorted(files)))
        elif len(dirs) != 0 and len(files) == 0:
            for d in sorted(dirs):
                file_names += gather_files(target_dir + '/' + d)
        else:
            raise Exception('Something went wrong')
    return file_names


def glue_files(file_list, delay, save_to):
    read_files = list(map(lambda f: plt.imread(f), file_list))
    imageio.mimsave('{}.gif'.format(save_to), read_files, duration=delay)


def main(args):
    file_list = gather_files(args.folder)
    glue_files(file_list, args.delay, args.save_to)

parser = ArgumentParser(description="GifMaker Api",
                        formatter_class=RawTextHelpFormatter)

parser.add_argument("--folder",
                    type=str,
                    nargs='+',
                    help='Folders where files are located')

parser.add_argument("--delay",
                    type=float,
                    help='Delay')

parser.add_argument("--save_to",
                    type=str,
                    help='Saving name')

args = parser.parse_args()

main(args)
