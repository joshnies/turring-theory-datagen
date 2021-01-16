from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.generator import Generator


class JderegJavaUtilGenerator(Generator):
    """
    Case generator for "jdereg/java-util":
    https://github.com/jdereg/java-util
    """

    @staticmethod
    def generate(args, write):
        print('\nGenerating case data for "jdereg/java-util"')

        items = [
            gen_item(
                f'public class {gen_mask_token(0)}<{gen_mask_token(1)}, {gen_mask_token(2)}> implements {gen_mask_token(3)}<{gen_mask_token(4)}, {gen_mask_token(5)}> {{'
                f'class {gen_mask_token(0)} implements {gen_mask_token(3)} {{'
            ),
            gen_item(
                f'private final {gen_mask_token(0)}<{gen_mask_token(1)}, {gen_mask_token(2)}> {gen_mask_token(3)};'
                f'private {gen_mask_token(3)};'
            ),
            gen_item(
                f'if ({gen_mask_token(0)} == null) {{'
            ),
            gen_item(
                f'throw new IllegalArgumentException({gen_mask_token(0)});'
                f'throw new Error({gen_mask_token(0)});'
            ),
            gen_item(
                f'{gen_mask_token(0)} = {gen_mask_token(1)};'
            ),
            gen_item(
                f'{gen_mask_token(0)} = new HashSet<>();',
                f'{gen_mask_token(0)} = new Set();'
            ),
            gen_item(
                f'{gen_mask_token(0)} {gen_mask_token(1)} = {gen_mask_token(2)}.{gen_mask_token(3)}({gen_mask_token(4)});',
                f'let {gen_mask_token(1)} = {gen_mask_token(2)}.{gen_mask_token(3)}({gen_mask_token(4)});'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}(({gen_mask_token(2)}) {gen_mask_token(3)});',
                f'{gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(3)});'
            ),
            gen_item(
                f'public {gen_mask_token(0)} {gen_mask_token(1)}({gen_mask_token(2)} {gen_mask_token(3)}, {gen_mask_token(4)} {gen_mask_token(5)}) {{',
                f'const {gen_mask_token(1)} = ({gen_mask_token(3)}, {gen_mask_token(5)}) => {{'
            ),
            gen_item(
                f'return {gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(2)}, {gen_mask_token(3)});'
            ),
            gen_item(
                f'public boolean {gen_mask_token(0)}({gen_mask_token(1)} {gen_mask_token(2)}) {{',
                f'const {gen_mask_token(0)} => ({gen_mask_token(2)}) {{'
            ),
            gen_item(
                f'boolean {gen_mask_token(0)} = {gen_mask_token(1)}.{gen_mask_token(2)}({gen_mask_token(3)});'
                f'let {gen_mask_token(0)} = {gen_mask_token(1)}.{gen_mask_token(2)}({gen_mask_token(3)});'
            ),
            gen_item(
                f'public void {gen_mask_token(0)}(Map<? extends {gen_mask_token(1)}, ? extends {gen_mask_token(2)}> {gen_mask_token(3)}) {{',
                f'const {gen_mask_token(0)} => ({gen_mask_token(3)}) => {{'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(2)});'
            ),
            gen_item(
                f'return {gen_mask_token(0)} instanceof Map && {gen_mask_token(1)}.{gen_mask_token(2)}({gen_mask_token(3)});',
                f'return {gen_mask_token(0)} instanceof Object && {gen_mask_token(1)}.{gen_mask_token(2)}({gen_mask_token(3)});'
            ),
            gen_item(
                f'public Collection<{gen_mask_token(0)}> {gen_mask_token(1)}() {{',
                f'const {gen_mask_token(1)} = () => {{'
            ),
            gen_item(
                f'public Set<{gen_mask_token(0)}> {gen_mask_token(1)}() {{',
                f'const {gen_mask_token(1)} = () => {{'
            ),
            gen_item(
                f'public Set<Entry<{gen_mask_token(0)}, {gen_mask_token(1)}>> {gen_mask_token(2)}() {{',
                f'const {gen_mask_token(2)} = () => {{'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}().{gen_mask_token(2)}({gen_mask_token(3)});'
            ),
            gen_item(
                f'public void {gen_mask_token(0)}(Collection<{gen_mask_token(1)}> {gen_mask_token(2)}) {{',
                f'const {gen_mask_token(0)} = ({gen_mask_token(2)}) => {{'
            ),
            gen_item(
                f'public void {gen_mask_token(0)}({gen_mask_token(1)}<{gen_mask_token(2)}, {gen_mask_token(3)}> {gen_mask_token(4)}) {{',
                f'const {gen_mask_token(0)} = ({gen_mask_token(4)}) => {{'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(2)}.{gen_mask_token(3)});'
            ),
            gen_item(
                f'public Map {gen_mask_token(0)}() {{',
                f'const {gen_mask_token(0)} => () => {{'
            )
        ]

        for i in items:
            write(i)