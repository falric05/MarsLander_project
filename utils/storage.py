from pathlib import Path

###

ROOT = Path(__file__).parent.parent

class StorageClass:
    @property
    def _root_dir_name(self) -> str:
        return 'MarsLander_project'

    #

    @property
    def _root_dir(self) -> Path:
        return ROOT.joinpath(self._root_dir_name)

    ### data folder directory
    def data_dir(self) -> Path:
        return ROOT.joinpath("data")
    
    ### get data ins path
    def data_file_url(self, file_name: str) -> Path:
        assert isinstance(file_name, str)
        return self.data_dir().joinpath(f"{file_name}.txt")

    ### out folder directory
    def out_dir(self) -> Path:
        return ROOT.joinpath("out")

    ### out gif file path
    def out_file_url(self, file_name: str, sub_dir: str = None) -> Path:
        assert isinstance(file_name, str)
        assert sub_dir is None or isinstance(sub_dir, str)

        partial_file_url = f"{sub_dir}/" if sub_dir is not None else ""

        self.out_dir().joinpath(partial_file_url).mkdir(exist_ok=True, parents=True)
        return self.out_dir().joinpath(f"{partial_file_url}{file_name}.gif")

Storage = StorageClass()