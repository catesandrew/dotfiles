import json
import plistlib

from cairosvg import svg2png

outfile_path_reletive = './'


def make_png(basefile_svg, filename_dict):
    for description in filename_dict:
        # parse name and svg
        icon_svg = basefile_svg.replace('STRINGTOREPLACE21145',
                                        filename_dict[description])
        icon_name = description.replace(' ', '-').lower()
        icon_filepath = outfile_path_reletive + icon_name + '.png'
        # convert to png and save
        svg2png(bytestring=icon_svg, write_to=icon_filepath)


def parse_info_plist(info, filename_dict):
    for description in filename_dict:
        icon_name = description.replace(' ', '-').lower()
        for item in info['CFBundleDocumentTypes']:
            if item['CFBundleTypeName'] == description:
                item['CFBundleTypeIconFile'] = icon_name + '.icns'


def run():
    # description: text to be on icon
    filename_dict = {}
    with open('filename.json', 'r') as file1:
        filename_dict = json.load(file1)

    basefile_svg = ''
    with open('base-icon.svg', 'r') as file1:
        basefile_svg = file1.read()

    make_png(basefile_svg, filename_dict)

    # plist
    info = {}
    with open('info.plist', 'rb') as file1:
        info = plistlib.loads(file1.read())

    parse_info_plist(info, filename_dict)

    with open('Info.plist.new', 'wb') as file1:
        file1.write(plistlib.dumps(info))

    print('make-png done')


if __name__ == '__main__':
    run()
