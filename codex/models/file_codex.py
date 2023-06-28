"""
Module for the Codex class, which is used to generate a codex from a DFT input
"""

import os
import re
from abc import ABC, abstractmethod
import uuid

from tabulate import tabulate

from codex.utils import range_dict_get, remove_html_tags
from codex.database.utils import get_database

from codex.utils import generate_cdxid


class CodexTag:
    """
    Class for a codex tag, with formatting
    """

    def __init__(
        self,
        name,
        value,
        comment,
        tag_pad,
        value_pad,
        comment_pad,
        id,
        href,
        indent,
        comment_token=None,
    ):
        """
        Args:
            tag (str): the tag name, formatted as it will be printed
            value (str): the tag value, formatted
            comment (str): the tag comment, formatted
            tag_pad (str or int): number of spaces to pad the tag
            value_pad (str or int): number of spaces to pad the value
            comment_pad (str or int): number of spaces to pad the comment
            id (str): the tag id (unformatted)
            href (str): the tag href
            parent (subclass of AbstractCodex): the parent codex
        """
        self.name = name
        self.value = value
        self.comment = comment_token + comment if comment_token else comment
        self.tag_pad = tag_pad * " " if isinstance(tag_pad, int) else tag_pad
        self.value_pad = value_pad * " " if isinstance(value_pad, int) else value_pad
        self.comment_pad = comment_pad * " " if isinstance(comment_pad, int) else comment_pad
        self.id = id
        self.href = href
        self.indent = indent * " " if isinstance(indent, int) else indent

    def to_string(self, pretty=True, with_comment=True, split=False):
        """
        Print the tag, similar to the codex.html.j2 jinja template
        Args:
            pretty (bool): whether to print the tag in a pretty format
            with_comment (bool): whether to print the comment
            split (bool): whether to split the tag into [indent, name, rest].
                          For use in jinja so all the HTML logic can stay in templates

        """
        indent = self.indent
        tag_pad = self.tag_pad if pretty else " "
        value_pad = self.value_pad if pretty else " "
        comment_pad = self.comment_pad if pretty else " "
        comment = self.comment if with_comment else ""
        name = self.name
        value = self.value
        if split:
            return [indent, name, f"{tag_pad}={value_pad}{value}{comment_pad}{comment}"]
        return f"{indent}{name}{tag_pad}={value_pad}{value}{comment_pad}{comment}"


class AbstractFileCodex(ABC):
    """
    A class to store the parsed information from a DFT input file
    """

    def __init__(
        self,
        filename,
        raw_file,
        dbversion,
        **kwargs,
    ):
        """
        Construct a Codex. Generally, you'll either provide all the arguments except client,
        or skip all the optional ones and provide a client (a la Codex.from_file)
        Args:
            filename (str): the name of the input file
            raw_file (str): the raw input file
            dbversion (str): the database version. See documentation for details.
            _id (str): the id of the codex. If not provided, a random uuid will be generated.
            tags (dict): list of CodexTag objects for tags
            cards (str): string for the cards (to be implemented later)
            filetype (str): the file type of the input file
            client (pymongo.MongoClient): a pymongo client
        """
        print(f"Got kwargs {kwargs}")
        self.filename = filename
        self.raw_file = raw_file
        self.dbversion = dbversion
        self._id = kwargs.get("_id", None) or generate_cdxid("file")

        tags = kwargs.get("tags", None)
        cards = kwargs.get("cards", None)
        filetype = kwargs.get("filetype", None)
        print(f"Got tags {tags}, cards {cards}, filetype {filetype}")
        client = kwargs.get("client", None)
        # Can't just use all, tags might be empty dict and cards might be empty string
        if tags is not None and cards is not None and filetype is not None:
            self.tags = tags
            self.cards = cards
            self.filetype = filetype
        elif client:
            db, self.dbversion = get_database(client, self.code, dbversion)
            self.filetype = self._get_filetype(db)
            self.tags, self.cards = self._get_tags_cards(db[self.filetype])
        else:
            raise ValueError("Must provide either tags, cards, and filetype, or a Mongo client")

    @classmethod
    def from_file(cls, input_file, client, dbversion="latest"):
        """
        Construct a Codex from FileStorage object (input_file)
        """
        filename = input_file.filename
        raw_file = input_file.read().decode("utf-8")
        return cls(filename, raw_file, dbversion, client=client)

    @property
    @abstractmethod
    def code(self):
        """
        The code that the codex is for (e.g., vasp, qe, etc.)
        Short name, all lowercase, for internal use.
        """
        pass

    @property
    @abstractmethod
    def indent(self):
        """
        The indent to use for each line in the input file
        """
        pass

    @property
    @abstractmethod
    def comment_token(self):
        """
        The token to use for comments (e.g., ! or #)
        If you'd like to have a space after the token, include it like comment_token = '! '
        """
        pass

    @property
    @abstractmethod
    def section_start_token(self):
        """
        The token to use for the start of a section
        (e.g., & for F90 namelists like in QE)
        """
        pass

    @property
    @abstractmethod
    def section_end_token(self):
        """
        The token to use for the end of a section
        (e.g., / for F90 namelists like in QE)
        """
        pass

    @abstractmethod
    def _get_filetype(self):
        """
        Figures out what type of input file is being read
        (for codes that have multiple packages or read several files per run)
        """
        pass

    @abstractmethod
    def _get_tags_cards(self):
        """
        Gets the tags and cards from the input file
        returns:
            tags: a list of dictionaries, where each dictionary has the form:
            {
                "name": the name of the tag, formatted for display
                "value": the value of the tag, formatted for display
                "comment": the comment associated with the tag, formatted for display
                "tag_pad": the pad (spaces or whatever) between the tag and the '=' (or other) sign
                "value_pad": the pad between the '=' (or other) sign and the value
                "comment_pad": the pad between the value and the comment
                "id": used for the html tag-id. Should be unique to the tag,
                                         and unformatted (no spaces)
                "href": link to go to when the tag is clicked
            }
            cards: currently just a string appended at the end of the file,
                   will be implemented later
        """
        pass

    @abstractmethod
    def _format_value(self, tag, value):
        """
        Formats the value of a tag.
        For example, in VASP and QE, True and False are formatted as .TRUE. and .FALSE.
        In VASP, the value of MAGMOM gets special formatting.
        In QE, strings are formatted with single quotes.
        """
        pass

    @staticmethod
    @abstractmethod
    def _file_to_str(file_dict):
        """
        Given a dict-like "file_dict", it formats it as a string

        A "file_dict" is basically whatever the _get_tags_cards() implementation
        passes to the _get_tags method of the AbstractCodex. Generally this is some
        dict-like object (e.g., pymatgen.io.vasp.inputs.Incar or f90nml.namelist)
        """
        pass

    @abstractmethod
    def _get_href(self, tag):
        """
        Gets the href for a tag
        """
        pass

    def to_string(self, raw=False, pretty=True, with_comments=True):
        """
        Converts the codex to a string
        Args:
            raw (bool): if True, returns the raw file
            pretty (bool): if True, returns the file with the tags formatted with indentation
            with_comments (bool): if True, returns the file with comments
        """
        if raw:
            return self.raw_file
        file_str = ""
        for section, tags in self.tags.items():
            file_str += self.section_start_token + section + "\n" if section else ""
            for tag in tags:
                file_str += tag.to_string(pretty, with_comments)
                file_str += "\n"
            file_str += self.section_end_token + "\n" if section else ""
        file_str += self.cards
        return file_str.strip()

    def _get_tags(self, file_dict, db):
        """
        Converts the tags in the input file into the format needed for the codex
        Takes a dict-like file_dict object that looks like this:
        {section_name: {tag_name: tag_value, ...}, ...}
        """
        pads_and_formats = self._get_pad_and_format(self._file_to_str(file_dict))
        tags_dict = {}
        for section, tags in file_dict.items():
            tags_dict[section] = []
            for t, val in tags.items():
                comment = self._get_comment(t, val, db)
                href = self._get_href(t, db)
                # TODO: doesn't work with lists with multiple elements (QE)
                tags_dict[section].append(
                    CodexTag(
                        pads_and_formats[t]["formatted_tag"],
                        pads_and_formats[t]["formatted_value"],
                        comment,
                        pads_and_formats[t]["tag_pad"],
                        pads_and_formats[t]["value_pad"],
                        pads_and_formats[t]["comment_pad"],
                        t,
                        href,
                        self.indent,
                        comment_token=self.comment_token,
                    )
                )
        return tags_dict

    # TODO: this needs a lot of improvement
    def _get_comment(self, tag, val, db):
        """
        Gets the comment for the tag and value from the database
        """
        if options := db.find_one({"name": tag})["options"]:
            if comment := range_dict_get(str(val), options):
                return remove_html_tags(comment)
        if summary := db.find_one({"name": tag})["summary"]:
            comment = remove_html_tags(summary)
            if comment.startswith(tag):
                # TODO: this is vasp specific and temporary
                comment = comment.strip(tag).strip(", ").strip()
            return comment
        return "No Comment Available"

    def _get_pad_and_format(self, file_string):
        """
        Takes a file that has already been converted to a string (using, e.g., file_to_str)
        and formats the tags and values and computes the padding between them
        """
        lines = []
        for l in file_string.split("\n"):
            if l:
                tag = l.split("=")[0].strip()
                val = l.split("=")[1].strip()
                val = self._format_value(tag, val)
                comment = "! Place holder"
                lines.append([tag, "=", val, comment])
        table = tabulate(lines, tablefmt="plain")
        tags = {}
        for l in table.split("\n"):
            tag = l.split("=")[0]
            val = l.split("=")[1].split("!")[0]
            # The -1 accounts for the extra spacing around the = sign
            # that's being added by tabulate
            key = re.sub(r"\(.*\)", "", tag.strip())
            tags[key] = {
                "tag_pad": (len(tag) - len(tag.rstrip()) - 1),
                "value_pad": (len(val) - len(val.lstrip()) - 1),
                "comment_pad": (len(val) - len(val.rstrip()) - 1),
                "formatted_value": val.strip(),
                "formatted_tag": tag.strip(),
            }
        return tags
