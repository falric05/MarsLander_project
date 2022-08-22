from pathlib import Path

###

ROOT_SRC = Path(__file__).parent.parent
ROOT = Path(__file__).parent.parent.parent

class Storage:
    @property
    def _root_dir_name(self) -> str:
        return 'MarsLander_project'

    #

    @property
    def _root_dir(self) -> Path:
        return ROOT.joinpath(self._root_dir_name)

    def data_dir(self) -> Path:
        return ROOT.joinpath("data")

    def out_dir(self) -> Path:
        # return self._root_dir.joinpath("out/json")
        return ROOT.joinpath("out")

    def out_file_url(self, file_name: str, sub_dir: str = None) -> Path:
        assert isinstance(file_name, str)
        assert sub_dir is None or isinstance(sub_dir, str)

        partial_file_url = f"{sub_dir}/" if sub_dir is not None else ""

        self.out_dir().joinpath(partial_file_url).mkdir(exist_ok=True, parents=True)

        return self.out_dir().joinpath(f"{partial_file_url}{file_name}.gif")